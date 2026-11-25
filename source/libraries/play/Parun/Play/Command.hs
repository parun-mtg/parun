-- | `parun play`: a game whose seats are each the search bot or, with --json,
-- a program speaking the line protocol on standard input and output.
module Parun.Play.Command where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TextIO
import qualified GHC.Clock as Clock
import Numeric.Natural (Natural)
import qualified Parun.Codec.Scenario as Codec.Scenario
import qualified Parun.CommandLine as CommandLine
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.Game as Engine.Game
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command.Config as Config
import qualified Parun.Play.Command.Flag as Flag
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Record as Record
import qualified Parun.Play.Search as Search
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GraveyardOrder as GraveyardOrder
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Legality as Legality.Type
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Result as Result
import qualified Parun.Types.Scenario as Scenario
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.TypeLine as TypeLine
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: [String] -> IO ()
main arguments = do
  config <- CommandLine.configured Flag.usage Config.help (Flag.fromArguments arguments >>= uncurry Config.fromFlags)
  Monad.forM_ (NonEmpty.nonEmpty (Config.decks config)) $ \paths -> do
    let options = Config.options config
    registry <- Registry.defaultRoot >>= Registry.loadRoot >>= Registry.defaultRegistry
    lists <- traverse (orExit . load registry) paths
    format <- orExit (settle (PlayOptions.format options) (NonEmpty.toList (NonEmpty.zip paths lists)))
    prior <- priorOf registry options format
    play (Protocol.overLines exchange) options prior registry format (fmap DeckList.deck lists)

-- | A deck list file with no bad line, as `parun deck` reads it; or every
-- problem with it.
load :: Registry.Registry Identity.Identity -> FilePath -> IO (Either [Text.Text] DeckList.Listed)
load registry path = do
  bytes <- ByteString.readFile path
  pure $ case Encoding.decodeUtf8' bytes of
    Left err -> Left [Text.pack (path <> ": not valid UTF-8: " <> show err)]
    Right contents -> loadText registry path contents

-- | A deck list's text with no bad line, as 'load' reads a file, its
-- problems labelled `label`, a name no card has with the registry's nearest
-- (DeckList.nearest); or every problem with it.
loadText :: Registry.Registry Identity.Identity -> FilePath -> Text.Text -> Either [Text.Text] DeckList.Listed
loadText registry label contents = case Identity.runIdentity (DeckList.parse registry contents) of
  Left corpus -> Left [Text.pack (Exception.displayException corpus)]
  Right listed -> case DeckList.sound listed of
    Left problems -> Left (fmap (\p -> Text.pack (label <> ": ") <> DeckList.explain near p) problems)
    Right _ -> Right listed
  where
    near = DeckList.nearest (either (const []) (fmap (CardName.MkCardName . DeckList.written)) (Identity.runIdentity (Registry.cards registry)))

-- | Every problem with a deck list's text, unlabelled, as a panel it is
-- pasted into shows them: a bad line, or a breach of the format its header
-- names ('settle'); none for a deck that can start a game.
check :: Registry.Registry Identity.Identity -> Text.Text -> IO [Text.Text]
check registry contents =
  fmap (unlabelled label) <$> case loadText registry label contents of
    Left problems -> pure problems
    Right listed -> Either.fromLeft [] <$> settle Nothing [(label, listed)]
  where
    label = "deck"

-- | A problem without the label 'loadText' or 'settle' put before it.
unlabelled :: FilePath -> Text.Text -> Text.Text
unlabelled label problem = Maybe.fromMaybe problem (Text.stripPrefix (Text.pack (label <> ": ")) problem)

-- | The format a game between these lists is under, by name: the one asked
-- for, else the one their headers name, checking every deck against it; or
-- why the game can't start, which it can't when two headers name different
-- formats and none is asked for (see docs/decisions.md, "The format of a game
-- whose lists' headers disagree").
settle :: Maybe Text.Text -> [(FilePath, DeckList.Listed)] -> IO (Either [Text.Text] (Maybe Format.Format))
settle asked lists = case gameFormat asked lists of
  Left problem -> pure (Left [problem])
  Right name -> do
    violations <- refusals name lists
    if null violations
      then fmap (maybe (Right Nothing) (Bifunctor.bimap pure Just)) (traverse Legality.shipped name)
      else pure (Left violations)

-- | The name of the format a game between these lists is under: the one asked
-- for, else the one their headers name; or why there is none.
gameFormat :: Maybe Text.Text -> [(FilePath, DeckList.Listed)] -> Either Text.Text (Maybe Text.Text)
gameFormat asked lists = case (asked, headers) of
  (Just _, _) -> Right asked
  (Nothing, []) -> Right Nothing
  (Nothing, (_, named) : rest)
    | all ((== named) . snd) rest -> Right (Just named)
    | otherwise -> Left (Text.intercalate (Text.pack ", ") [Text.pack (path <> " names ") <> name | (path, name) <- headers] <> Text.pack "; name one with --format")
  where
    headers = [(path, name) | (path, listed) <- lists, Just (_, name) <- [DeckList.format listed]]

-- | Why the game can't start: a list's header names a format other than the
-- one asked for, or its deck breaks the rules or pool of the format asked for
-- or its header names, in a game not for ante, naming every violation of
-- every deck as `parun deck` does; and, under a format, every violation of
-- the rules a game states of its decks together (Parun.Legality.checkGame).
refusals :: Maybe Text.Text -> [(FilePath, DeckList.Listed)] -> IO [Text.Text]
refusals format lists = do
  reports <- traverse (\(path, listed) -> fmap (fmap (Text.pack (path <> ": ") <>) . snd) (Legality.report format Nothing listed)) lists
  let together = if Maybe.isJust format then fmap Legality.explain (Legality.checkGame GameSettings.plain (fmap (DeckList.deck . snd) lists)) else []
  pure (concat reports <> together)

-- | How `parun play`'s search bot deals what it cannot see: as 'inferring'
-- says, else from the game's `format`'s pool (Search.pooled), since outside
-- tournament play an opponent's list is not known.
priorOf :: Registry.Registry Identity.Identity -> PlayOptions.PlayOptions -> Maybe Format.Format -> IO Search.Prior
priorOf registry options format = Maybe.fromMaybe (poolIn registry format) (inferring registry (PlayOptions.meta options) (PlayOptions.pool options))

-- | The search bot dealing from the decks of the meta file at `meta`
-- (Search.inferred), else from the pool of the format named `pool`
-- (Search.pooled); Nothing when neither is given.
inferring :: Registry.Registry Identity.Identity -> Maybe FilePath -> Maybe Text.Text -> Maybe (IO Search.Prior)
inferring registry meta pool = case (meta, pool) of
  (Just path, _) -> Just (Search.inferred . fmap (fmap (DeckList.deck . snd)) <$> loadMeta registry path)
  (Nothing, Just name) -> Just (poolIn registry . Just =<< orExit (fmap (Bifunctor.first pure) (Legality.shipped name)))
  (Nothing, Nothing) -> Nothing

-- | The search bot dealing from `format`'s pool among the registry's cards,
-- by the shipped legality table ('pooled'); or the problem on standard error
-- and a failed exit.
poolIn :: Registry.Registry Identity.Identity -> Maybe Format.Format -> IO Search.Prior
poolIn registry format = do
  cards <- orExit (pure (Bifunctor.bimap (pure . Text.pack . Exception.displayException) (fmap Right) (Identity.runIdentity (Registry.cards registry))))
  orExit (pooled (Legality.tablePath >>= Legality.loadTable) format cards)

-- | The search bot dealing from `format`'s pool among `cards` ('poolOf'), or
-- why it can't: the table unread, or no card in the pool.
pooled :: IO (Either Text.Text LegalityTable.LegalityTable) -> Maybe Format.Format -> [Either Text.Text Card.Card] -> IO (Either [Text.Text] Search.Prior)
pooled table format cards = fmap (>>= maybe (Left [Text.pack "no card in the pool"]) Right . Search.pooled) (poolOf table format cards)

-- | Each of `cards` as a candidate of `format`'s pool (Search.pooled): its
-- printing if it is in the pool, else Nothing, read only once asked. The pool
-- is the cards of the Oracle card reference legal or restricted in the
-- format's pool, every one where it has none or there is no format, but those
-- no deck holds: a nontraditional card (CR 108.2a), Attractions and
-- Contraptions among them (Engine.Game.isNontraditional), and a conspiracy
-- (CR 315.3). `table`, the legality table, is read only for a pool.
poolOf :: IO (Either Text.Text LegalityTable.LegalityTable) -> Maybe Format.Format -> [Either Text.Text Card.Card] -> IO (Either [Text.Text] (Seq.Seq (Maybe Printing.Printing)))
poolOf table format cards = do
  standing <- case Format.pool =<< format of
    Nothing -> pure (Right (const True))
    Just key -> fmap (fmap (\t card -> Legality.poolStanding t key card `elem` [Just (Just Legality.Type.Legal), Just (Just Legality.Type.Restricted)])) table
  let library card = not (Card.synthetic card) && not (Engine.Game.isNontraditional card) && all (Set.notMember CardType.Conspiracy . TypeLine.types . Face.typeLine) (Card.faces card)
      candidate legal = either (const Nothing) (\card -> if library card && legal card then Just (Printing.ofCard card) else Nothing)
  pure (either (Left . pure) (\legal -> Right (Seq.fromList (fmap (candidate legal) cards))) standing)

-- | `parun play`'s game ('game'), its result said, its seed drawn where none
-- is given and said, and its scenario saved where asked.
play :: Protocol.Exchange IO -> PlayOptions.PlayOptions -> Search.Prior -> Registry.Registry Identity.Identity -> Maybe Format.Format -> NonEmpty.NonEmpty Deck.Deck -> IO ()
play send options prior registry format decks = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  seed <- maybe Key.fresh pure (PlayOptions.seed options)
  (outcome, recorded) <- game (Text.pack "parun play") send (pure Nothing) (const (pure ())) [] (pure ()) options prior registry format decks seed
  Monad.forM_ (PlayOptions.save options) (`save` recorded)
  case outcome of
    Right (result, _) -> (if PlayOptions.json options then TextIO.hPutStrLn IO.stderr else TextIO.putStrLn) (Wording.result Nothing result)
    Left reason -> TextIO.hPutStrLn IO.stderr (Text.pack "Stopped: " <> reason <> Text.pack ".")
  TextIO.hPutStrLn IO.stderr (Text.pack "Seed " <> Key.toHex seed <> Text.pack ".")

-- | A game played with `client` among the decks, seat 1's first, under
-- `format`, the one 'settle' names (CR 100.2), from the generator keyed by
-- `seed`, its scenario's description naming both: its result and final state,
-- or why it stopped first (Game.run), the end of input before a seat chose
-- who plays first among them; and the scenario it replays as, up to where it
-- stopped. With --json, each line protocol request goes out through
-- `send` and its reply comes back: over standard input and output in `parun
-- play` ('exchange'), as values in the web client. Each bot seat deals what
-- it cannot see by `prior`, and its answer is taken from `stored` while it
-- has one, and otherwise given to `kept` ('answerer'), its search waiting
-- on `breathe` at each step and timed by 'timed' at the think limit.
-- `watchers` see every prompt too.
game :: Text.Text -> Protocol.Exchange IO -> IO (Maybe R.Reply) -> (R.Reply -> IO ()) -> [Observer.Observer (Game.Play IO)] -> IO () -> PlayOptions.PlayOptions -> Search.Prior -> Registry.Registry Identity.Identity -> Maybe Format.Format -> NonEmpty.NonEmpty Deck.Deck -> Key.Type.Key -> IO (Either Text.Text (Result.Result, GameState.GameState), Scenario.Scenario)
game client send stored kept watchers breathe options prior registry format decks seed = do
  let reference :: Registry.Registry IO
      reference = Registry.hoist (pure . Identity.runIdentity) registry
  let count = Natural.length decks
      numbers = NonEmpty.fromList [1 .. count]
      ask chooser among = fmap (fmap ((+ 1) . PlayerId.unwrap)) (Protocol.playOrDraw send (idOf chooser) (fmap idOf among))
  (starter, generator) <- State.runStateT (Game.starter (TextIO.hPutStrLn IO.stderr) ask numbers options) (Generator.fromKey seed)
  let orders = Map.fromList [(idOf n, GraveyardOrder.Matters) | n <- Set.toList (PlayOptions.arrangesGraveyard options)]
      record = saved (Registry.printings registry) (Text.pack "Played with " <> client <> Text.pack ", seed " <> Key.toHex seed <> Text.pack ".") format orders
  case starter of
    -- No seat chose who plays first: the game never began.
    Nothing -> pure (Left (Text.pack "end of input"), record (seated 1 decks) Seq.empty)
    Just first -> do
      memories <- IORef.newIORef Map.empty
      aims <- IORef.newIORef Map.empty
      let matchup = seated first decks
          seats = Map.fromList [(idOf n, answerer reference send stored kept (IORef.readIORef memories) (Search.keptBy (IORef.readIORef aims) (IORef.modifyIORef' aims) (idOf n)) breathe (timed (PlayOptions.thinkLimit options)) options prior n) | n <- NonEmpty.toList numbers]
      entries <- IORef.newIORef Seq.empty
      let observers = [Record.recording (labels count) (Game.underneath . IORef.modifyIORef' entries . flip (Seq.|>)), Search.remembering (IORef.modifyIORef' memories)] <> watchers
          interrupted e = case e of
            Exception.UserInterrupt -> pure (Left (Text.pack "interrupted"))
            _ -> Exception.throwIO e
      outcome <- Exception.catch (Game.run reference format (PlayOptions.limits options) observers seats generator orders matchup) interrupted
      timeline <- IORef.readIORef entries
      pure (outcome, record matchup timeline)

-- | Who answers seat `n`: the search bot at its seat's budget and weights,
-- dealing what it cannot see by `prior`, remembering what its seat's memory
-- in `memories` holds (Search.remembering), keeping its line in `aim`, if
-- it is a bot's, its search waiting on `breathe` at each step and timed by
-- `clock` (Search.search), its answers taken from `stored` and given to
-- `kept` (Game.remembered); otherwise `send`.
answerer :: (Monad m) => Registry.Registry m -> Protocol.Exchange m -> m (Maybe R.Reply) -> (R.Reply -> m ()) -> m (Map.Map PlayerId.PlayerId Search.Memory) -> Search.Aim m -> m () -> Search.Clock m -> PlayOptions.PlayOptions -> Search.Prior -> Natural -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
answerer registry send stored kept memories aim breathe clock options prior n
  | Just (budget, weights) <- Map.lookup n (PlayOptions.bots options) = Game.remembered stored kept (Search.search registry budget weights prior (pure []) (Game.underneath (fmap (Map.findWithDefault Search.blank (idOf n)) memories)) aim (Game.underneath breathe) clock)
  | otherwise = Game.wired registry send

-- | A decision's clock running out once `limit` milliseconds have passed
-- since it started; at 0, never.
timed :: Natural -> Search.Clock IO
timed limit
  | limit == 0 = Search.unhurried
  | otherwise = do
      start <- Game.underneath Clock.getMonotonicTimeNSec
      pure (Game.underneath (fmap (\now -> toInteger (now - start) >= toInteger limit * 1000000) Clock.getMonotonicTimeNSec))

idOf :: Natural -> PlayerId.PlayerId
idOf n = PlayerId.MkPlayerId (n - 1)

-- | Every seat with its deck, numbered from 1 in turn order, the seat
-- numbered `first` at the head, which plays first (Game.run).
seated :: Natural -> NonEmpty.NonEmpty Deck.Deck -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
seated first decks =
  let numbered = NonEmpty.zip (NonEmpty.fromList [1 :: Natural ..]) decks
      (before, from) = NonEmpty.break ((== first) . fst) numbered
   in fmap (Bifunctor.first idOf) (Maybe.fromMaybe numbered (NonEmpty.nonEmpty (from <> before)))

-- | The decks of the meta file at `path`, each with its share and its path as
-- the file names it, or exits saying why it holds none.
loadMeta :: Registry.Registry Identity.Identity -> FilePath -> IO (NonEmpty.NonEmpty (Natural, (Text.Text, DeckList.Listed)))
loadMeta registry path = do
  meta <- orExit (readMeta path)
  traverse (\(share, name) -> (,) share . (,) name <$> orExit (load registry (relativeTo path (Text.unpack name)))) meta

-- | The meta file at `path`, or why it holds none.
readMeta :: FilePath -> IO (Either [Text.Text] (NonEmpty.NonEmpty (Natural, Text.Text)))
readMeta path = do
  bytes <- ByteString.readFile path
  pure $ case Encoding.decodeUtf8' bytes of
    Left err -> Left [Text.pack (path <> ": not valid UTF-8: " <> show err)]
    Right contents -> case parseMeta contents of
      Left problems -> Left (fmap (Text.pack (path <> ": ") <>) problems)
      Right meta -> Right meta

-- | A meta file's decks, each with its share: one deck list's path a line,
-- after a share (a positive whole number) and a space, or alone for a share
-- of 1. A blank line and one starting `#` say nothing. Or every bad line.
parseMeta :: Text.Text -> Either [Text.Text] (NonEmpty.NonEmpty (Natural, Text.Text))
parseMeta contents =
  let entry :: (Natural, Text.Text) -> Either Text.Text (Maybe (Natural, Text.Text))
      entry (number, line) =
        let stripped = Text.strip line
            (first, rest) = Text.break (== ' ') stripped
            path = Text.strip rest
         in case Read.readMaybe (Text.unpack first) of
              _ | Text.null stripped || Text.isPrefixOf (Text.pack "#") stripped -> Right Nothing
              Just 0 | not (Text.null path) -> Left (Text.pack ("line " <> show number <> ": a share of 0"))
              Just share | not (Text.null path) -> Right (Just (share, path))
              _ -> Right (Just (1, stripped))
      parsed :: ([Text.Text], [Maybe (Natural, Text.Text)])
      parsed = Either.partitionEithers (fmap entry (zip [1 ..] (Text.lines contents)))
      (problems, entries) = parsed
   in case (problems, NonEmpty.nonEmpty (Maybe.catMaybes entries)) of
        ([], Just meta) -> Right meta
        ([], Nothing) -> Left [Text.pack "no deck"]
        _ -> Left problems

-- | `path` read from the directory holding `meta`, unless it is absolute.
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo meta path = case (path, List.dropWhileEnd (/= '/') meta) of
  ('/' : _, _) -> path
  (_, directory) -> directory <> path

-- | Each of so many seats' label in a saved game.
labels :: Natural -> Map.Map PlayerId.PlayerId Label.Label
labels count = Map.fromList [(idOf n, Label.MkLabel (Text.pack ("player" <> show n))) | n <- [1 .. count]]

-- | The scenario a game played from this matchup under `format`, each seat at
-- its graveyard order in `orders` and its answers' entries recorded in
-- `timeline`, replays as.
saved :: PrintingTable.PrintingTable -> Text.Text -> Maybe Format.Format -> Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Seq.Seq Timed.Timed -> Scenario.Scenario
saved table description format orders matchup = Record.scenario table description format (fmap (\(pid, deck) -> (pid, Record.labelOf (labels (Natural.length matchup)) pid, Map.findWithDefault GraveyardOrder.Indifferent pid orders, deck)) matchup)

-- | Writes a scenario to `path`.
save :: FilePath -> Scenario.Scenario -> IO ()
save path scenario = do
  ByteString.writeFile path (Encoding.encodeUtf8 (rendered scenario))
  IO.hPutStrLn IO.stderr ("Saved " <> path <> ".")

-- | A scenario as its file holds it.
rendered :: Scenario.Scenario -> Text.Text
rendered scenario = Common.render (Codec.encode Codec.Scenario.codec scenario) <> Text.pack "\n"

-- | One request line out on standard output, its reply line back; Nothing at
-- the end of input.
exchange :: Text.Text -> IO (Maybe Text.Text)
exchange line = do
  TextIO.putStrLn line
  IO.hFlush IO.stdout
  done <- IO.isEOF
  if done then pure Nothing else fmap Just TextIO.getLine

-- | The answer, or its problems on standard error and a failed exit.
orExit :: IO (Either [Text.Text] a) -> IO a
orExit action = action >>= either (\problems -> mapM_ (TextIO.hPutStrLn IO.stderr) problems >> Exit.exitFailure) pure
