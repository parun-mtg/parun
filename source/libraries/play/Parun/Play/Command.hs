-- | `parun play`: a game of two seats at one terminal, either of them the
-- random bot, or both over the line protocol with --json.
module Parun.Play.Command where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock.System as Clock
import Numeric.Natural (Natural)
import qualified Parun.Codec.Scenario as Codec.Scenario
import qualified Parun.DeckList as DeckList
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Interpreter as Interpreter
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Legality as Legality
import qualified Parun.Play.Client as Client
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Record as Record
import qualified Parun.Play.Render as Render
import qualified Parun.Play.SplitMix as SplitMix.Play
import qualified Parun.Registry as Registry
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.GraveyardOrder as GraveyardOrder
import qualified Parun.Types.Hotseat as Hotseat
import qualified Parun.Types.Label as Label
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintingTable as PrintingTable
import qualified Parun.Types.Scenario as Scenario
import qualified Parun.Types.SplitMix as SplitMix
import qualified Parun.Types.Terminal as Terminal
import qualified Parun.Types.Timed as Timed
import qualified System.Console.GetOpt as GetOpt
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: [String] -> IO ()
main arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, [one, two], []) -> case foldr (=<<) (Right PlayOptions.none) updates of
    Left problem -> usage [problem]
    Right options -> do
      registry <- Registry.defaultRegistry
      lists <- (,) <$> load registry one <*> load registry two
      refuse (PlayOptions.format options) [(one, fst lists), (two, snd lists)]
      play options registry (Bifunctor.bimap DeckList.deck DeckList.deck lists)
  (_, _, problems) -> usage problems

flags :: [GetOpt.OptDescr (PlayOptions.PlayOptions -> Either String PlayOptions.PlayOptions)]
flags =
  [ GetOpt.Option [] ["seed"] (GetOpt.ReqArg (\s o -> maybe (Left ("not a seed: " <> s)) (\n -> Right o {PlayOptions.seed = Just n}) (Read.readMaybe s)) "N") "the generator's seed; the clock's when absent",
    GetOpt.Option [] ["first"] (GetOpt.ReqArg (\s o -> fmap (\n -> o {PlayOptions.first = Just n}) (seat s)) "1|2") "which seat plays first; a seat picked at random chooses when absent",
    GetOpt.Option [] ["bot"] (GetOpt.ReqArg (\s o -> fmap (\n -> o {PlayOptions.bots = Set.insert n (PlayOptions.bots o)}) (seat s)) "1|2") "a seat the random bot plays",
    GetOpt.Option [] ["json"] (GetOpt.NoArg (\o -> Right o {PlayOptions.json = True})) "speak the line protocol on standard input and output",
    GetOpt.Option [] ["save"] (GetOpt.ReqArg (\s o -> Right o {PlayOptions.save = Just s}) "FILE") "save the game as a scenario, even one cut short",
    GetOpt.Option [] ["no-auto-pass"] (GetOpt.NoArg (\o -> Right o {PlayOptions.autoPass = False})) "ask at every priority, even when passing and mana abilities are all there is",
    GetOpt.Option [] ["arrange-graveyard"] (GetOpt.ReqArg (\s o -> fmap (\n -> o {PlayOptions.arrangesGraveyard = Set.insert n (PlayOptions.arrangesGraveyard o)}) (seat s)) "1|2") "a seat asked to order cards put into its graveyard at the same time",
    GetOpt.Option [] ["format"] (GetOpt.ReqArg (\s o -> Right o {PlayOptions.format = Just (Text.pack s)}) "FORMAT") "refuse a deck that is not legal in this format: constructed, limited, or one with a pool, such as vintage"
  ]
  where
    seat s = case Read.readMaybe s of
      Just n | n == 1 || n == 2 -> Right n
      _ -> Left ("not a seat: " <> s)

usage :: [String] -> IO a
usage problems = do
  mapM_ (IO.hPutStr IO.stderr) problems
  IO.hPutStr IO.stderr (GetOpt.usageInfo "usage: parun play [OPTION...] DECK DECK" flags)
  Exit.exitFailure

-- | A deck list file with no bad line, as `parun deck` reads it.
load :: Registry.Registry Identity.Identity -> FilePath -> IO DeckList.Listed
load registry path = do
  bytes <- ByteString.readFile path
  contents <- either (\err -> failWith (path <> ": not valid UTF-8: " <> show err)) pure (Encoding.decodeUtf8' bytes)
  case Identity.runIdentity (DeckList.parse registry contents) of
    Left corpus -> failWith (Exception.displayException corpus)
    Right listed -> case DeckList.sound listed of
      Left problems -> do
        mapM_ (TextIO.hPutStrLn IO.stderr . (\p -> Text.pack (path <> ": ") <> DeckList.explain p)) problems
        Exit.exitFailure
      Right _ -> pure listed

-- | Stops before the game when a list's header names a format other than the
-- one asked for, or its deck breaks the rules or pool of the format asked for
-- or its header names, in a game not for ante, naming every violation of
-- every deck as `parun deck` does.
refuse :: Maybe Text.Text -> [(FilePath, DeckList.Listed)] -> IO ()
refuse format lists = do
  reports <- traverse (\(path, listed) -> fmap (fmap (Text.pack (path <> ": ") <>) . snd) (Legality.report format listed)) lists
  let violations = concat reports
  Monad.unless (null violations) $ do
    mapM_ (TextIO.hPutStrLn IO.stderr) violations
    Exit.exitFailure

-- | Nanoseconds since the epoch; on macOS only microseconds tick.
nanoseconds :: Clock.SystemTime -> Natural
nanoseconds t = Integer.toNaturalSaturating (toInteger (Clock.systemSeconds t) * 1000000000 + toInteger (Clock.systemNanoseconds t))

play :: PlayOptions.PlayOptions -> Registry.Registry Identity.Identity -> (Deck.Deck, Deck.Deck) -> IO ()
play options registry (one, two) = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  let reference :: Registry.Registry IO
      reference = Registry.hoist (pure . Identity.runIdentity) registry
      lookUp = Identity.runIdentity . Interpreter.chosenCard registry
  seed <- maybe (fmap (SplitMix.Play.fromClock . nanoseconds) Clock.getSystemTime) pure (PlayOptions.seed options)
  IO.hPutStrLn IO.stderr ("Seed " <> show seed <> ".")
  let people = filter (`Set.notMember` PlayOptions.bots options) [1, 2]
  let (tell, ask)
        | PlayOptions.json options = (TextIO.hPutStrLn IO.stderr, Protocol.playOrDraw exchange . idOf)
        | otherwise = (TextIO.putStrLn, Game.playOrDraw terminal)
  (starter, generator) <- State.runStateT (Game.starter tell ask options) (SplitMix.MkSplitMix seed)
  first <- maybe (failWith "end of input") pure starter
  let matchup = seated first (one, two)
      answerer n
        | Set.member n (PlayOptions.bots options) = Game.bot reference
        | PlayOptions.json options = Game.wired reference exchange
        | otherwise = Game.human reference terminal lookUp
      seats = Map.fromList [(idOf n, answerer n) | n <- [1, 2]]
      screen = Hotseat.fresh {Hotseat.autoPass = PlayOptions.autoPass options}
      hotseat = case people of
        [only] -> screen {Hotseat.facing = Just (idOf only)}
        _ -> screen
  entries <- IORef.newIORef Seq.empty
  let orders = Map.fromList [(idOf n, GraveyardOrder.Matters) | n <- Set.toList (PlayOptions.arrangesGraveyard options)]
      observers = [Record.recording labels (Game.underneath . IORef.modifyIORef' entries . flip (Seq.|>)) | Maybe.isJust (PlayOptions.save options)]
      interrupted e = case e of
        Exception.UserInterrupt -> pure (Left (Text.pack "interrupted"))
        _ -> Exception.throwIO e
      finish = if PlayOptions.json options then const (pure ()) else Client.ending terminal (fmap idOf people)
  outcome <- Exception.catch (Game.run reference observers seats finish hotseat generator orders matchup) interrupted
  Monad.forM_ (PlayOptions.save options) $ \path -> do
    timeline <- IORef.readIORef entries
    save path (saved (Registry.printings registry) (Text.pack ("Played with parun play, seed " <> show seed <> ".")) orders matchup timeline)
  case outcome of
    Right (result, _) -> (if PlayOptions.json options then TextIO.hPutStrLn IO.stderr else TextIO.putStrLn) (Render.result result)
    Left reason -> TextIO.hPutStrLn IO.stderr (Text.pack "Stopped: " <> reason <> Text.pack ".")

idOf :: Natural -> PlayerId.PlayerId
idOf n = PlayerId.MkPlayerId (n - 1)

-- | Both seats with their decks, the seat numbered `first` at the head, which
-- plays first (Game.run).
seated :: Natural -> (Deck.Deck, Deck.Deck) -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
seated first (one, two)
  | first == 2 = (idOf 2, two) NonEmpty.:| [(idOf 1, one)]
  | otherwise = (idOf 1, one) NonEmpty.:| [(idOf 2, two)]

-- | Each seat's label in a saved game.
labels :: Map.Map PlayerId.PlayerId Label.Label
labels = Map.fromList [(idOf n, Label.MkLabel (Text.pack ("player" <> show n))) | n <- [1, 2 :: Natural]]

-- | The scenario a game played from this matchup, each seat at its graveyard
-- order in `orders` and its answers' entries recorded in `timeline`, replays
-- as.
saved :: PrintingTable.PrintingTable -> Text.Text -> Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Seq.Seq Timed.Timed -> Scenario.Scenario
saved table description orders matchup = Record.scenario table description (fmap (\(pid, deck) -> (Record.labelOf labels pid, Map.findWithDefault GraveyardOrder.Indifferent pid orders, deck)) matchup)

-- | Writes a scenario to `path`.
save :: FilePath -> Scenario.Scenario -> IO ()
save path scenario = do
  ByteString.writeFile path (Encoding.encodeUtf8 (Common.render (Codec.encode Codec.Scenario.codec scenario) <> Text.pack "\n"))
  IO.hPutStrLn IO.stderr ("Saved " <> path <> ".")

terminal :: Terminal.Terminal IO
terminal =
  Terminal.MkTerminal
    { Terminal.say = TextIO.putStrLn,
      Terminal.listen = do
        IO.hFlush IO.stdout
        done <- IO.isEOF
        if done then pure Nothing else fmap Just TextIO.getLine,
      Terminal.clear = TextIO.putStr (Text.pack "\ESC[2J\ESC[H")
    }

-- | One request line out on standard output, its reply line back.
exchange :: Text.Text -> IO (Maybe Text.Text)
exchange line = TextIO.putStrLn line >> Terminal.listen terminal

failWith :: String -> IO a
failWith problem = IO.hPutStrLn IO.stderr problem >> Exit.exitFailure
