-- | `parun meta`: a deck, or two versions of one, played against every deck
-- of a meta, and how often it won each matchup and overall, weighted by each
-- deck's share, with 95% confidence intervals.
module Parun.Play.Meta where

import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Functor.Identity as Identity
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Match as Match
import qualified Parun.Play.Search as Search
import qualified Parun.Registry as Registry
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Weights as Weights
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO

main :: [String] -> IO ()
main arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, metaPath : one : rest, []) | length rest <= 1 -> case Monad.foldM (\options update -> update options) MatchOptions.none updates of
    Left problem -> usage [problem]
    Right options
      | MatchOptions.games options == 0 -> usage ["no games to play\n"]
      | otherwise -> do
          registry <- Registry.defaultRoot >>= Registry.loadRoot >>= Registry.defaultRegistry
          versions <- traverse (\path -> (,) path <$> Command.orExit (Command.load registry path)) (one NonEmpty.:| rest)
          opponents <- Command.loadMeta registry metaPath
          prior <- Match.priorOf registry options
          format <- Command.orExit (Command.settle Nothing (NonEmpty.toList versions <> NonEmpty.toList (fmap (\(_, (name, list)) -> (Text.unpack name, list)) opponents)))
          searchers <- Command.orExit (fmap (Bifunctor.first pure) (Match.searchersOf options))
          run options searchers prior registry format (fmap (Bifunctor.bimap Text.pack DeckList.deck) versions) (fmap (\(share, (name, list)) -> (share, (name, DeckList.deck list))) opponents)
  (_, _, problems) -> usage problems

-- | `parun match`'s options but `--save`, seat 1 being the deck tried and
-- seat 2 the meta's.
flags :: [GetOpt.OptDescr (MatchOptions.MatchOptions -> Either String MatchOptions.MatchOptions)]
flags = filter (\(GetOpt.Option _ long _ _) -> notElem "save" long) Match.flags

usage :: [String] -> IO a
usage problems = do
  mapM_ (IO.hPutStr IO.stderr) problems
  IO.hPutStr IO.stderr (GetOpt.usageInfo "usage: parun meta [OPTION...] META DECK [DECK]" flags)
  Exit.exitFailure

-- | Plays every version against every deck of the meta under `format`,
-- saying how each game ended on standard error, then the report on standard
-- output.
run :: MatchOptions.MatchOptions -> Map.Map Natural (SearchBudget.SearchBudget, Weights.Weights) -> Search.Prior -> Registry.Registry Identity.Identity -> Maybe Format.Format -> NonEmpty.NonEmpty (Text.Text, Deck.Deck) -> NonEmpty.NonEmpty (Natural, (Text.Text, Deck.Deck)) -> IO ()
run options searchers prior registry format versions meta = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  seed <- maybe Key.fresh pure (MatchOptions.seed options)
  played <-
    Monad.forM (NonEmpty.zip (0 NonEmpty.:| [1 ..]) meta) $ \(index, (share, (name, opponent))) -> do
      wins <-
        Monad.forM versions $ \(version, deck) -> do
          let outcomes = Identity.runIdentity (matchup registry format searchers prior seed (MatchOptions.games options) index deck opponent)
          Monad.forM_ (zip [1 :: Natural ..] outcomes) $ \(n, outcome) ->
            TextIO.hPutStrLn IO.stderr (version <> Text.pack " against " <> name <> Text.pack (", game " <> show n <> ": ") <> Match.ended outcome)
          pure (fmap won outcomes)
      pure (share, name, wins)
  mapM_ TextIO.putStrLn (report (fmap fst versions) played)
  TextIO.hPutStrLn IO.stderr (Text.pack "Seed " <> Key.toHex seed <> Text.pack ".")

-- | The `games` games, from game 0, the deck in seat 1 plays against the
-- `index`th deck of a meta from `seed`, as `parun match` plays them from that
-- deck's key (Match.seedAt): every version meets a deck over the same seeds,
-- so a difference between versions is not a difference between draws.
matchup :: (Monad m) => Registry.Registry m -> Maybe Format.Format -> Map.Map Natural (SearchBudget.SearchBudget, Weights.Weights) -> Search.Prior -> Key.Type.Key -> Natural -> Natural -> Deck.Deck -> Deck.Deck -> m [Either Text.Text (Result.Result, GameState.GameState)]
matchup registry format searchers prior seed games index deck opponent =
  traverse (\n -> fst <$> Match.game registry format searchers prior (Match.seedAt seed index) n (Match.both (deck, opponent))) (takeWhile (< games) [0 ..])

-- | Whether seat 1 won; a draw or a stop is not a win.
won :: Either Text.Text (Result.Result, GameState.GameState) -> Bool
won outcome = case outcome of
  Right (Result.Won pid, _) -> pid == Command.idOf 1
  _ -> False

-- | The report on the versions named, each meta deck with its share, its name
-- and each version's wins game by game: each version's rate against each
-- deck and overall, and with two versions the first's lead over the second.
report :: NonEmpty.NonEmpty Text.Text -> NonEmpty.NonEmpty (Natural, Text.Text, NonEmpty.NonEmpty [Bool]) -> [Text.Text]
report versions played =
  let total = sum (fmap (\(share, _, _) -> share) played)
      weight share = if total == 0 then 0 else toRational share / toRational total
      line version text = Text.pack "  " <> version <> Text.pack ": " <> text
      lead = case NonEmpty.toList versions of
        [one, two] -> Just (Text.pack "  " <> one <> Text.pack " minus " <> two <> Text.pack ", over the same seeds: ")
        _ -> Nothing
      pairs wins = case NonEmpty.toList wins of
        [one, two] -> zip one two
        _ -> []
      against (share, name, wins) =
        (Text.pack "Against " <> name <> Text.pack (" (share " <> show share <> " of " <> show total <> "):"))
          : NonEmpty.toList (NonEmpty.zipWith (\version w -> line version (single w)) versions wins)
            <> foldMap (\prefix -> [prefix <> points (weighted (-1, 1) ((1, difference (pairs wins)) NonEmpty.:| []))]) lead
      columns = NonEmpty.transpose (fmap (\(share, _, wins) -> fmap ((,) (weight share)) wins) played)
      overall =
        Text.pack "Overall, weighted by share:"
          : NonEmpty.toList (NonEmpty.zipWith (\version column -> line version (percent (weighted (0, 1) (fmap (fmap rate) column)))) versions columns)
            <> foldMap (\prefix -> [prefix <> points (weighted (-1, 1) (fmap (\(share, _, wins) -> (weight share, difference (pairs wins))) played))]) lead
   in Text.pack "Win rates with 95% confidence intervals."
        : concatMap against (NonEmpty.toList played) <> overall

-- | A matchup's wins out of its games, with its rate and Wilson interval.
single :: [Bool] -> Text.Text
single wins =
  let k = Natural.length (filter id wins)
      n = Natural.length wins
   in Text.pack ("won " <> show k <> " of " <> show n <> " games, ") <> percent (wilson k n)

-- | An estimate of a rate and its interval.
type Estimate = (Rational, Rational, Rational)

-- | A rate as a percentage and its interval, each to a tenth.
percent :: Estimate -> Text.Text
percent (estimate, low, high) = tenths False estimate <> Text.pack "% (" <> tenths False low <> Text.pack "% to " <> tenths False high <> Text.pack "%)."

-- | A difference of rates in percentage points and its interval, each signed.
points :: Estimate -> Text.Text
points (estimate, low, high) = tenths True estimate <> Text.pack " points (" <> tenths True low <> Text.pack " to " <> tenths True high <> Text.pack ")."

-- | A fraction as a percentage to a tenth, its sign always said if `signed`.
tenths :: Bool -> Rational -> Text.Text
tenths signed x =
  let t = round (x * 1000) :: Integer
      sign
        | t < 0 = "-"
        | signed = "+"
        | otherwise = ""
   in Text.pack (sign <> show (div (abs t) 10) <> "." <> show (mod (abs t) 10))

-- | The normal quantile of a two-sided 95% interval.
z :: Rational
z = 196 / 100

-- | `wins` of `games` games' rate and its Wilson score interval.
wilson :: Natural -> Natural -> Estimate
wilson wins games = case games of
  0 -> (0, 0, 1)
  _ ->
    let n = toRational games
        p = toRational wins / n
        scale = 1 + z * z / n
        centre = (p + z * z / (2 * n)) / scale
        half = z * squareRoot (p * (1 - p) / n + z * z / (4 * n * n)) / scale
     in (p, max 0 (centre - half), min 1 (centre + half))

-- | A rate's variance estimate: Agresti and Coull's, two wins and two losses
-- added, so a matchup won or lost every game still counts as uncertain.
rate :: [Bool] -> (Rational, Rational)
rate wins =
  let n = toRational (Natural.length wins)
      k = toRational (Natural.length (filter id wins))
      adjusted = (k + 2) / (n + 4)
   in (if n == 0 then 0 else k / n, adjusted * (1 - adjusted) / (n + 4))

-- | Paired games' mean difference, the first's win minus the second's, and
-- the variance of that mean, estimated with one pair the first won alone and
-- one the second did added, so paired games that all agree still count as
-- uncertain.
difference :: [(Bool, Bool)] -> (Rational, Rational)
difference pairs =
  let ds :: [Rational]
      ds = fmap (\(one, two) -> score one - score two) pairs
      score b = if b then 1 else 0
      n = toRational (Natural.length ds)
      adjusted = 1 : -1 : ds
      m = n + 2
      centre = sum adjusted / m
      spread = sum (fmap (\d -> (d - centre) * (d - centre)) adjusted) / (m - 1)
   in (if n == 0 then 0 else sum ds / n, spread / m)

-- | A weighted sum of independent estimates, each with its variance, and its
-- normal interval, clamped to `bounds`.
weighted :: (Rational, Rational) -> NonEmpty.NonEmpty (Rational, (Rational, Rational)) -> Estimate
weighted (lowest, highest) parts =
  let estimate = sum (fmap (\(w, (e, _)) -> w * e) parts)
      half = z * squareRoot (sum (fmap (\(w, (_, v)) -> w * w * v) parts))
   in (estimate, max lowest (estimate - half), min highest (estimate + half))

-- | A non-negative number's square root to a millionth.
squareRoot :: Rational -> Rational
squareRoot x =
  let scaled = floor (x * 1000000000000) :: Integer
      go r = let next = div (r + div scaled r) 2 in if next >= r then r else go next
   in toRational (if scaled <= 0 then 0 else go scaled) / 1000000
