-- | `parun match`: many seeded games between two decks, the random bot in both
-- seats, and how often each seat won.
module Parun.Play.Match where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Functor.Identity as Identity
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock.System as Clock
import qualified Data.Word as Word
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Record as Record
import qualified Parun.Play.Render as Render
import qualified Parun.Play.SplitMix as SplitMix.Play
import qualified Parun.Registry as Registry
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Hotseat as Hotseat
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SplitMix as SplitMix
import qualified Parun.Types.Tally as Tally
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Timed as Timed
import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: [String] -> IO ()
main arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, [one, two], []) -> case foldr (=<<) (Right MatchOptions.none) updates of
    Left problem -> usage [problem]
    Right options -> do
      registry <- Registry.defaultRegistry
      lists <- (,) <$> Command.load registry one <*> Command.load registry two
      Command.refuse Nothing [(one, fst lists), (two, snd lists)]
      match options registry (Bifunctor.bimap DeckList.deck DeckList.deck lists)
  (_, _, problems) -> usage problems

flags :: [GetOpt.OptDescr (MatchOptions.MatchOptions -> Either String MatchOptions.MatchOptions)]
flags =
  [ GetOpt.Option [] ["seed"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a seed: " <> s)) (\n -> Right o {MatchOptions.seed = Just n}) (Read.readMaybe s)) "N") "the first game's seed, each later game's one more; the clock's when absent",
    GetOpt.Option [] ["games"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a number of games: " <> s)) (\n -> Right o {MatchOptions.games = n}) (Read.readMaybe s)) "N") "how many games to play; 100 when absent",
    GetOpt.Option [] ["save"] (GetOpt.ReqArg (\s o -> Right o {MatchOptions.save = Just s}) "DIR") "save each game in this directory as a scenario"
  ]

usage :: [String] -> IO a
usage problems = do
  mapM_ (IO.hPutStr IO.stderr) problems
  IO.hPutStr IO.stderr (GetOpt.usageInfo "usage: parun match [OPTION...] DECK DECK" flags)
  Exit.exitFailure

-- | Plays the games one after another, saying how each ended on standard
-- error, then the tally on standard output.
match :: MatchOptions.MatchOptions -> Registry.Registry Identity.Identity -> (Deck.Deck, Deck.Deck) -> IO ()
match options registry decks = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  seed <- maybe (fmap (SplitMix.Play.fromClock . Command.nanoseconds) Clock.getSystemTime) pure (MatchOptions.seed options)
  IO.hPutStrLn IO.stderr ("Seed " <> show seed <> ".")
  Monad.forM_ (MatchOptions.save options) (Directory.createDirectoryIfMissing True)
  let count = MatchOptions.games options
  tally <-
    Monad.foldM
      ( \tally index -> do
          let (outcome, timeline) = Identity.runIdentity (game registry seed index decks)
          TextIO.hPutStrLn IO.stderr (Text.pack ("Game " <> show (index + 1) <> ", seed " <> show (seedAt seed index) <> ", seat " <> show (firstAt index) <> " first: ") <> ended outcome)
          Monad.forM_ (MatchOptions.save options) $ \directory ->
            Command.save (directory <> "/game-" <> show (index + 1) <> ".json") (Command.saved (Registry.printings registry) (Text.pack ("Played with parun match, seed " <> show (seedAt seed index) <> ", seat " <> show (firstAt index) <> " first.")) Nothing Map.empty (Command.seated (firstAt index) decks) timeline)
          pure (tallied outcome tally)
      )
      Tally.empty
      (takeWhile (< count) [0 ..])
  mapM_ TextIO.putStrLn (summary count tally)

-- | Game `index`, from zero, of a match from `seed`: seeded 'seedAt', seat
-- 'firstAt' playing first, the random bot in both seats; with the entry each
-- answer replays as (Record.recording). It is the game `parun play --bot 1
-- --bot 2` plays at that seed and `--first`.
game :: (Monad m) => Registry.Registry m -> Word.Word64 -> Natural -> (Deck.Deck, Deck.Deck) -> m (Either Text.Text (Result.Result, GameState.GameState), Seq.Seq Timed.Timed)
game registry seed index decks =
  State.runStateT (Game.run (lifted registry) Nothing [Record.recording Command.labels sink] (Map.fromList [(Command.idOf n, Game.bot (lifted registry)) | n <- [1, 2]]) (const (pure ())) Hotseat.fresh (SplitMix.MkSplitMix (seedAt seed index)) Map.empty (Command.seated (firstAt index) decks)) Seq.empty

-- | The registry, beneath the entries kept so far.
lifted :: (Monad m) => Registry.Registry m -> Registry.Registry (State.StateT (Seq.Seq Timed.Timed) m)
lifted = Registry.hoist Trans.lift

-- | Keeps an answer's entry, after those before it.
sink :: (Monad m) => Timed.Timed -> Game.Play (State.StateT (Seq.Seq Timed.Timed) m) ()
sink entry = Game.underneath (State.modify' (Seq.|> entry))

-- | Game `index`'s seed: the match's, plus the index, wrapping.
seedAt :: Word.Word64 -> Natural -> Word.Word64
seedAt seed index = seed + Maybe.fromMaybe 0 (Natural.toWord64 (mod index (2 ^ (64 :: Natural))))

-- | The seat playing first in game `index`: seat 1 at an even index, seat 2
-- at an odd one, so each deck plays first in half the games. Each game is a
-- match of its own (CR 103.1), so no loser of a previous game chooses.
firstAt :: Natural -> Natural
firstAt index = if even index then 1 else 2

-- | The tally with one more game counted, a game with a result at the turn it
-- ended in.
tallied :: Either Text.Text (Result.Result, GameState.GameState) -> Tally.Tally -> Tally.Tally
tallied outcome tally = case outcome of
  Right (result, gs) -> tally {Tally.results = Map.insertWith (+) result 1 (Tally.results tally), Tally.turns = Tally.turns tally + GameState.turnNumber gs}
  Left _ -> tally {Tally.stopped = Tally.stopped tally + 1}

-- | How one game ended.
ended :: Either Text.Text (Result.Result, GameState.GameState) -> Text.Text
ended outcome = case outcome of
  Right (result, gs) -> Render.result result <> Text.pack (" Turn " <> show (GameState.turnNumber gs) <> ".")
  Left reason -> Text.pack "stopped: " <> reason <> Text.pack "."

-- | The tally of `count` games: each seat's wins, any team's, the draws and
-- stops, and the mean turns of a game with a result, to a tenth.
summary :: Natural -> Tally.Tally -> [Text.Text]
summary count tally =
  [Text.pack ("Seat " <> show n <> " won " <> show (won (Result.Won (Command.idOf n))) <> " of " <> show count <> " games.") | n <- [1, 2]]
    <> [Text.pack ("Team " <> show (TeamId.unwrap team) <> " won " <> show k <> " of " <> show count <> " games.") | (Result.TeamWon team, k) <- Map.toList (Tally.results tally)]
    <> [ Text.pack (show (won Result.Drawn) <> " drawn, " <> show (Tally.stopped tally) <> " stopped."),
         Text.pack (mean (sum (Tally.results tally)))
       ]
  where
    won result = Map.findWithDefault 0 result (Tally.results tally)
    mean finished
      | finished == 0 = "No game reached a result."
      | otherwise =
          let tenths = div (Tally.turns tally * 20 + finished) (2 * finished)
           in "A game with a result lasted " <> show (div tenths 10) <> "." <> show (mod tenths 10) <> " turns on average."
