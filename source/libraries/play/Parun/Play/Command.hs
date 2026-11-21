-- | `parun play`: a game whose seats are each the random bot or, with --json,
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
import Numeric.Natural (Natural)
import qualified Parun.Codec.Scenario as Codec.Scenario
import qualified Parun.DeckList as DeckList
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Legality as Legality
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Record as Record
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GraveyardOrder as GraveyardOrder
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.Result as Result
import qualified Parun.Types.Scenario as Scenario
import qualified Parun.Types.Timed as Timed
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: [String] -> IO ()
main arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, paths@(_ : _ : _), []) -> case foldr (=<<) (Right PlayOptions.none) updates of
    Left problem -> usage [problem]
    Right options
      | Just n <- List.find (> List.genericLength paths) (Maybe.maybeToList (PlayOptions.first options) <> Set.toList (PlayOptions.bots options <> PlayOptions.arrangesGraveyard options)) -> usage ["not a seat: " <> show n <> "\n"]
      | Just n <- unplayed options (List.genericLength paths) -> usage ["seat " <> show n <> " has no player: name it with --bot, or speak the line protocol with --json\n"]
      | otherwise -> do
          registry <- Registry.defaultRoot >>= Registry.loadRoot >>= Registry.defaultRegistry
          lists <- traverse (orExit . load registry) paths
          format <- orExit (settle (PlayOptions.format options) (zip paths lists))
          play (Protocol.overLines exchange) options registry format (fmap DeckList.deck (NonEmpty.fromList lists))
  (_, _, problems) -> usage problems

flags :: [GetOpt.OptDescr (PlayOptions.PlayOptions -> Either String PlayOptions.PlayOptions)]
flags =
  [ GetOpt.Option [] ["seed"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a seed: " <> s)) (\k -> Right o {PlayOptions.seed = Just k}) (Key.fromHex (Text.pack s))) "KEY") "the generator's key, 64 hexadecimal digits, said when the game ends; drawn from the operating system when absent",
    GetOpt.Option [] ["first"] (GetOpt.ReqArg (\s o -> fmap (\n -> o {PlayOptions.first = Just n}) (seat s)) "SEAT") "which seat plays first; a seat picked at random chooses when absent",
    GetOpt.Option [] ["bot"] (GetOpt.ReqArg (\s o -> fmap (\n -> o {PlayOptions.bots = Set.insert n (PlayOptions.bots o)}) (seat s)) "SEAT") "a seat the random bot plays",
    GetOpt.Option [] ["json"] (GetOpt.NoArg (\o -> Right o {PlayOptions.json = True})) "speak the line protocol on standard input and output",
    GetOpt.Option [] ["save"] (GetOpt.ReqArg (\s o -> Right o {PlayOptions.save = Just s}) "FILE") "save the game as a scenario, even one cut short",
    GetOpt.Option [] ["arrange-graveyard"] (GetOpt.ReqArg (\s o -> fmap (\n -> o {PlayOptions.arrangesGraveyard = Set.insert n (PlayOptions.arrangesGraveyard o)}) (seat s)) "SEAT") "a seat asked to order cards put into its graveyard at the same time",
    GetOpt.Option [] ["format"] (GetOpt.ReqArg (\s o -> Right o {PlayOptions.format = Just (Text.pack s)}) "FORMAT") "play under this format, refusing a deck not legal in it: constructed, limited, or one with a pool, such as vintage",
    GetOpt.Option [] ["object-limit"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a number of objects: " <> s)) (\n -> Right o {PlayOptions.limits = (PlayOptions.limits o) {Limits.objects = n}}) (Read.readMaybe s)) "N") ("the most objects the game may hold before a player with priority may only pass; " <> show (Limits.objects Limits.standard) <> " when absent")
  ]
  where
    seat s = case Read.readMaybe s of
      Just n | n >= 1 -> Right n
      _ -> Left ("not a seat: " <> s)

-- | The first seat of so many that nobody plays: one not the bot's, in a game
-- not over the line protocol.
unplayed :: PlayOptions.PlayOptions -> Natural -> Maybe Natural
unplayed options count
  | PlayOptions.json options = Nothing
  | otherwise = List.find (`Set.notMember` PlayOptions.bots options) [1 .. count]

usage :: [String] -> IO a
usage problems = do
  mapM_ (IO.hPutStr IO.stderr) problems
  IO.hPutStr IO.stderr (GetOpt.usageInfo "usage: parun play [OPTION...] DECK DECK [DECK...]" flags)
  Exit.exitFailure

-- | A deck list file with no bad line, as `parun deck` reads it; or every
-- problem with it.
load :: Registry.Registry Identity.Identity -> FilePath -> IO (Either [Text.Text] DeckList.Listed)
load registry path = do
  bytes <- ByteString.readFile path
  pure $ case Encoding.decodeUtf8' bytes of
    Left err -> Left [Text.pack (path <> ": not valid UTF-8: " <> show err)]
    Right contents -> loadText registry path contents

-- | A deck list's text with no bad line, as 'load' reads a file, its
-- problems labelled `label`; or every problem with it.
loadText :: Registry.Registry Identity.Identity -> FilePath -> Text.Text -> Either [Text.Text] DeckList.Listed
loadText registry label contents = case Identity.runIdentity (DeckList.parse registry contents) of
  Left corpus -> Left [Text.pack (Exception.displayException corpus)]
  Right listed -> case DeckList.sound listed of
    Left problems -> Left (fmap (\p -> Text.pack (label <> ": ") <> DeckList.explain p) problems)
    Right _ -> Right listed

-- | Every problem with a deck list's text, labelled `label`: a bad line, or
-- a breach of the format its header names ('settle'); none for a deck that
-- can start a game.
check :: Registry.Registry Identity.Identity -> FilePath -> Text.Text -> IO [Text.Text]
check registry label contents = case loadText registry label contents of
  Left problems -> pure problems
  Right listed -> Either.fromLeft [] <$> settle Nothing [(label, listed)]

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

-- | `parun play`'s game ('game'), its result said, its seed drawn where none
-- is given and said, and its scenario saved where asked.
play :: Protocol.Exchange IO -> PlayOptions.PlayOptions -> Registry.Registry Identity.Identity -> Maybe Format.Format -> NonEmpty.NonEmpty Deck.Deck -> IO ()
play send options registry format decks = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  seed <- maybe Key.fresh pure (PlayOptions.seed options)
  (outcome, recorded) <- game send options registry format decks seed
  Monad.forM_ (PlayOptions.save options) (`save` recorded)
  case outcome of
    Right (result, _) -> (if PlayOptions.json options then TextIO.hPutStrLn IO.stderr else TextIO.putStrLn) (Wording.result result)
    Left reason -> TextIO.hPutStrLn IO.stderr (Text.pack "Stopped: " <> reason <> Text.pack ".")
  TextIO.hPutStrLn IO.stderr (Text.pack "Seed " <> Key.toHex seed <> Text.pack ".")

-- | A game among the decks, seat 1's first, under `format`, the one 'settle'
-- names (CR 100.2), from the generator keyed by `seed`: its result and final state,
-- or why it stopped first (Game.run), the end of input before a seat chose
-- who plays first among them; and the scenario it replays as, up to where it
-- stopped. With --json, each line protocol request goes out through
-- `send` and its reply comes back: over standard input and output in `parun
-- play` ('exchange'), as values in the web client.
game :: Protocol.Exchange IO -> PlayOptions.PlayOptions -> Registry.Registry Identity.Identity -> Maybe Format.Format -> NonEmpty.NonEmpty Deck.Deck -> Key.Type.Key -> IO (Either Text.Text (Result.Result, GameState.GameState), Scenario.Scenario)
game send options registry format decks seed = do
  let reference :: Registry.Registry IO
      reference = Registry.hoist (pure . Identity.runIdentity) registry
  let count = Natural.length decks
      numbers = NonEmpty.fromList [1 .. count]
      ask chooser among = fmap (fmap ((+ 1) . PlayerId.unwrap)) (Protocol.playOrDraw send (idOf chooser) (fmap idOf among))
  (starter, generator) <- State.runStateT (Game.starter (TextIO.hPutStrLn IO.stderr) ask numbers options) (Generator.fromKey seed)
  let orders = Map.fromList [(idOf n, GraveyardOrder.Matters) | n <- Set.toList (PlayOptions.arrangesGraveyard options)]
      record = saved (Registry.printings registry) (Text.pack "Played with parun play, seed " <> Key.toHex seed <> Text.pack ".") format orders
  case starter of
    -- No seat chose who plays first: the game never began.
    Nothing -> pure (Left (Text.pack "end of input"), record (seated 1 decks) Seq.empty)
    Just first -> do
      let matchup = seated first decks
          answerer n
            | Set.member n (PlayOptions.bots options) = Game.bot reference
            | otherwise = Game.wired reference send
          seats = Map.fromList [(idOf n, answerer n) | n <- NonEmpty.toList numbers]
      entries <- IORef.newIORef Seq.empty
      let observers = [Record.recording (labels count) (Game.underneath . IORef.modifyIORef' entries . flip (Seq.|>))]
          interrupted e = case e of
            Exception.UserInterrupt -> pure (Left (Text.pack "interrupted"))
            _ -> Exception.throwIO e
      outcome <- Exception.catch (Game.run reference format (PlayOptions.limits options) observers seats generator orders matchup) interrupted
      timeline <- IORef.readIORef entries
      pure (outcome, record matchup timeline)

idOf :: Natural -> PlayerId.PlayerId
idOf n = PlayerId.MkPlayerId (n - 1)

-- | Every seat with its deck, numbered from 1 in turn order, the seat
-- numbered `first` at the head, which plays first (Game.run).
seated :: Natural -> NonEmpty.NonEmpty Deck.Deck -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
seated first decks =
  let numbered = NonEmpty.zip (NonEmpty.fromList [1 :: Natural ..]) decks
      (before, from) = NonEmpty.break ((== first) . fst) numbered
   in fmap (Bifunctor.first idOf) (Maybe.fromMaybe numbered (NonEmpty.nonEmpty (from <> before)))

-- | Each of so many seats' label in a saved game.
labels :: Natural -> Map.Map PlayerId.PlayerId Label.Label
labels count = Map.fromList [(idOf n, Label.MkLabel (Text.pack ("player" <> show n))) | n <- [1 .. count]]

-- | The scenario a game played from this matchup under `format`, each seat at
-- its graveyard order in `orders` and its answers' entries recorded in
-- `timeline`, replays as.
saved :: PrintingTable.PrintingTable -> Text.Text -> Maybe Format.Format -> Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Seq.Seq Timed.Timed -> Scenario.Scenario
saved table description format orders matchup = Record.scenario table description format (fmap (\(pid, deck) -> (Record.labelOf (labels (Natural.length matchup)) pid, Map.findWithDefault GraveyardOrder.Indifferent pid orders, deck)) matchup)

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
