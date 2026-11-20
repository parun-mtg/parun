-- | The web client as one loop (#9075): the page, the screen it shows, and
-- the data loaded so far, built chunk by chunk (Parun.Web.Loading) one
-- batch at a time while no tap waits.
module Parun.Browser.Shell where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Browser.Replay as Replay
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Engine.Quantity as Quantity
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Key as Key
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Key as Key
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Loading as Loading
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Screen as Screen
import qualified System.Environment as Environment

data Shell = MkShell
  { page :: Browser.Page Screen.Event,
    loaded :: IORef.IORef Loaded,
    -- | How much the page answers for the player at first
    -- (Web.Question.Level).
    level :: Web.Question.Level
  }

-- | The data so far: which chunks arrived, and what has been built from
-- them, each absent until built.
data Loaded = MkLoaded
  { loading :: Loading.Loading,
    -- | Each deck's file, by its name (Lobby.deckName).
    deckFiles :: Maybe (Map.Map Text.Text FilePath),
    -- | The format each deck's header names, by name.
    formats :: Map.Map Text.Text Text.Text,
    -- | Every card file, parsed only when looked up.
    cards :: Maybe (LazyMap.Map FilePath (Either Text.Text Card.Card)),
    -- | Each deck's colours, by name, as read so far.
    palette :: Map.Map Text.Text (Set.Set Color.Color),
    -- | The decks whose colours are still to read.
    unread :: [(Text.Text, FilePath)],
    registry :: Maybe (Registry.Registry Identity.Identity)
  }

-- | What woke a screen's loop.
data Woke
  = MkTapped Screen.Event Text.Text
  | -- | The data changed; the screen redraws.
    MkChanged

-- | The page mounted, the home screen drawn at once, and the loop run; an
-- exception that escapes is shown on the page. A replay the page names
-- (parun_scenario) skips the home screen, and a rematch's decks
-- (parun_mine, parun_theirs, parun_seed) go straight to that game.
start :: IO ()
start = do
  named <- Environment.lookupEnv "parun_scenario"
  case named of
    Just name -> Replay.start mempty name
    Nothing -> do
      chosen <- maybe Web.Question.MkStandard (Maybe.fromMaybe Web.Question.MkStandard . Web.Question.levelNamed) <$> Environment.lookupEnv "parun_level"
      shell <- MkShell <$> Browser.mount <*> IORef.newIORef (MkLoaded Loading.new Nothing Map.empty Nothing Map.empty [] Nothing) <*> pure chosen
      Exception.catch (opening shell >> run shell) $ \problem ->
        Browser.display (page shell) $ message ["The page stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

-- | The game a rematch link names, if it names one whose decks exist.
opening :: Shell -> IO ()
opening shell = do
  asked <- (,,) <$> Environment.lookupEnv "parun_mine" <*> Environment.lookupEnv "parun_theirs" <*> Environment.lookupEnv "parun_seed"
  case asked of
    (Just mine, Just theirs, seed) -> do
      files <- decksRead shell
      let names = Text.pack mine NonEmpty.:| Text.splitOn (Text.pack ",") (Text.pack theirs)
      Monad.forM_ (files >>= \known -> traverse (\name -> (,) name <$> Map.lookup name known) names) $ \seats ->
        play shell seats (seed >>= Key.fromHex . Text.pack)
    _ -> pure ()

run :: Shell -> IO ()
run shell = do
  outcome <- home shell False
  already <- Loading.arrived . loading <$> IORef.readIORef (loaded shell)
  case outcome of
    Home.MkGo Home.MkPlayBots -> lobby shell >> run shell
    Home.MkGo Home.MkReplays -> Replay.start already ""
    Home.MkBack -> run shell
    Home.MkStay _ -> run shell

-- | The home screen until it leads somewhere, a game paused or not.
home :: Shell -> Bool -> IO Home.Outcome
home shell paused = go Home.new {Home.paused = paused}
  where
    go shown = do
      now <- IORef.readIORef (loaded shell)
      let current = shown {Home.cards = fmap Natural.length (cards now), Home.problem = Loading.problem (loading now)}
      Browser.display (page shell) (fmap Screen.MkHome (Home.view current))
      woke <- await shell
      case woke of
        MkTapped (Screen.MkHome event) _ -> case Home.update event current of
          Home.MkStay next -> go next
          outcome -> pure outcome
        _ -> go current

-- | The lobby until it goes home, filling in as decks and cards arrive; a
-- game it starts is played, and ends, from here.
lobby :: Shell -> IO ()
lobby shell = do
  need shell Loading.MkDecks
  need shell Loading.MkCards
  go Lobby.new
  where
    go shown = do
      current <- refreshed shell shown
      Browser.display (page shell) (fmap Screen.MkLobby (Lobby.view current))
      woke <- await shell
      case woke of
        MkTapped (Screen.MkLobby event) value -> case event of
          Lobby.MkHome -> pure ()
          Lobby.MkStart | Just mine <- Lobby.mine current -> do
            others <- traverse (maybe (randomOf (Lobby.opponents current)) (pure . Just)) (Lobby.theirs current)
            files <- Maybe.fromMaybe Map.empty <$> decksRead shell
            case traverse (\name -> (,) name <$> Map.lookup name files) (mine NonEmpty.:| Maybe.catMaybes others) of
              Just seats -> play shell seats Nothing
              Nothing -> go current
          Lobby.MkRandom -> do
            picked <- randomOf (if Lobby.choosing current == Lobby.MkMine then Lobby.decks current else Lobby.opponents current)
            go (maybe current (\name -> Lobby.update (Lobby.MkPick name) Text.empty current) picked)
          _ -> go (Lobby.update event value current)
        _ -> go current

-- | The lobby with what has loaded since: the decks once read, their
-- colours so far, the card count, and the lists of the decks chosen.
refreshed :: Shell -> Lobby.Lobby -> IO Lobby.Lobby
refreshed shell shown = do
  now <- IORef.readIORef (loaded shell)
  let withDecks = case deckFiles now of
        Just files | Lobby.reading shown -> Lobby.withDecks (Seq.fromList (Map.keys files)) (formats now) shown
        _ -> shown
      wanted = [name | name <- Maybe.catMaybes (Lobby.mine withDecks : Lobby.theirs withDecks), Map.notMember name (Lobby.previews withDecks)]
  read' <- case (cards now, deckFiles now) of
    (Just byFile, Just files) -> do
      root <- Registry.defaultRoot
      Monad.forM wanted $ \name -> do
        contents <- maybe (pure Text.empty) (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) (Map.lookup name files)
        pure (name, [lineOf root byFile n card | (n, card) <- Lobby.entries contents])
    _ -> pure []
  pure
    withDecks
      { Lobby.cardFiles = fmap Natural.length (cards now),
        Lobby.palette = palette now,
        Lobby.previews = Map.union (Lobby.previews withDecks) (Map.fromList read')
      }

-- | A card of a deck's list, as the lobby previews it.
lineOf :: FilePath -> LazyMap.Map FilePath (Either Text.Text Card.Card) -> Natural -> CardName.CardName -> Lobby.Line
lineOf root byFile n name = case LazyMap.lookup (Registry.cardPath root (Registry.slugFor name)) byFile of
  Just (Right c)
    | face <- NonEmpty.head (Card.faces c) ->
        let types = TypeLine.types (Face.typeLine face)
            kind
              | Set.member CardType.Land types = Lobby.MkLand
              | Set.member CardType.Creature types = Lobby.MkCreature
              | otherwise = Lobby.MkSpell
         in Lobby.MkLine n name (Face.manaCost face) (Quantity.manaValueOf face) kind
  _ -> Lobby.MkLine n name Nothing 0 Lobby.MkSpell

-- | One of these decks at random.
randomOf :: Seq.Seq Text.Text -> IO (Maybe Text.Text)
randomOf among = do
  key <- Key.fresh
  let roll = foldr (\w acc -> acc * 4294967296 + toInteger w) 0 (Key.unwrap key)
  pure (Integer.toInt (mod roll (toInteger (max 1 (Seq.length among)))) >>= (`Seq.lookup` among))

-- | The deck files, once read; nothing if the decks failed to load.
decksRead :: Shell -> IO (Maybe (Map.Map Text.Text FilePath))
decksRead shell = either (const Nothing) Just <$> untilBuilt shell [Loading.MkDecks] (deckFiles <$> IORef.readIORef (loaded shell))

-- | Waits, loading meanwhile, until the value is built, taps waiting; or
-- why one of the needs it is built from did not load.
untilBuilt :: Shell -> [Loading.Need] -> IO (Maybe a) -> IO (Either Text.Text a)
untilBuilt shell needs built = do
  mapM_ (need shell) needs
  let go = do
        done <- built
        now <- loading <$> IORef.readIORef (loaded shell)
        case done of
          Just a -> pure (Right a)
          Nothing
            | any (\n -> Maybe.isJust (Loading.blocked n now)) needs -> pure (Left (Maybe.fromMaybe Text.empty (Loading.problem now)))
            | otherwise -> do
                progressed <- awaitWork shell
                Monad.unless progressed Browser.idle
                go
  go

-- | A game between these decks, once the registry is built, shown while it
-- builds; then how it ended.
play :: Shell -> NonEmpty.NonEmpty (Text.Text, FilePath) -> Maybe Key.Key -> IO ()
play shell seats seed = do
  Browser.display (page shell) (Hotseat.starting (fmap fst seats))
  built <- untilBuilt shell [Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
  case built of
    Left problem -> Browser.display (page shell) (Hotseat.message [problem])
    Right reg -> do
      ended <- Hotseat.game (page shell) reg (level shell) seats seed
      Browser.display (page shell) (either Hotseat.message Ending.view ended)
  -- The ending's links leave the page.
  Monad.forever (Browser.next (page shell))

-- | A tap waiting, else a chunk arrived, else one batch of loading work,
-- else whatever comes next: a waiting tap is never kept behind work.
await :: Shell -> IO Woke
await shell = do
  waiting <- Browser.pending (page shell)
  if waiting
    then uncurry MkTapped <$> Browser.next (page shell)
    else do
      progressed <- awaitWork shell
      if progressed
        then pure MkChanged
        else do
          input <- Browser.nextInput (page shell)
          case input of
            Browser.MkTap event value -> pure (MkTapped event value)
            Browser.MkArrival a -> arrived shell [a] >> pure MkChanged

-- | Folds in the chunks arrived, else does one batch of work and yields to
-- the page; whether either happened.
awaitWork :: Shell -> IO Bool
awaitWork shell = do
  chunks <- Browser.arrivals (page shell)
  if not (null chunks)
    then arrived shell chunks >> pure True
    else do
      worked <- work shell
      Monad.when worked Browser.pause
      pure worked

arrived :: Shell -> [Browser.Arrival] -> IO ()
arrived shell chunks = IORef.modifyIORef' (loaded shell) $ \l -> l {loading = foldr (Loading.arrive . arrival) (loading l) chunks}
  where
    arrival a = case a of
      Browser.MkArrived name -> Right name
      Browser.MkFailed name -> Left name

-- | Asks the worker for what a screen waits on, first.
need :: Shell -> Loading.Need -> IO ()
need shell wanted = do
  now <- IORef.readIORef (loaded shell)
  let (next, chunks) = Loading.demand wanted (loading now)
  IORef.writeIORef (loaded shell) now {loading = next}
  mapM_ Browser.want chunks

-- | One batch of loading work, if any is ready to do: the deck list, the
-- card list, eight decks' colours, or the registry, which goes before the
-- colours once a game waits on it.
work :: Shell -> IO Bool
work shell = do
  now <- IORef.readIORef (loaded shell)
  let ready n = Loading.ready n (loading now)
      gameWaits = Loading.MkRules `elem` Loading.wanted (loading now)
      buildRegistry = case cards now of
        Just byFile
          | ready Loading.MkRules,
            Maybe.isNothing (registry now) -> Just $ do
              built <- Registry.defaultRegistry (LazyMap.toList byFile)
              IORef.modifyIORef' (loaded shell) $ \l -> l {registry = Just built}
        _ -> Nothing
      readColours = case (cards now, unread now) of
        (Just byFile, _ : _) -> Just $ do
          root <- Registry.defaultRoot
          let (batch, rest) = splitAt 8 (unread now)
              colours name = case LazyMap.lookup (Registry.cardPath root (Registry.slugFor name)) byFile of
                Just (Right card) -> Lobby.cardColours card
                _ -> Set.empty
          read' <- Monad.forM batch $ \(name, path) -> do
            contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
            Exception.evaluate . (,) name $! Lobby.deckColours colours contents
          IORef.modifyIORef' (loaded shell) $ \l -> l {palette = Map.union (palette l) (Map.fromList read'), unread = rest}
        _ -> Nothing
      steps =
        [ Monad.guard (ready Loading.MkDecks && Maybe.isNothing (deckFiles now)) >> Just readDecks,
          Monad.guard (ready Loading.MkCards && Maybe.isNothing (cards now)) >> Just readCards
        ]
          <> (if gameWaits then [buildRegistry, readColours] else [readColours, buildRegistry])
  case Maybe.catMaybes steps of
    step : _ -> step >> pure True
    [] -> pure False
  where
    readDecks = do
      deckRoot <- Decks.defaultRoot
      paths <- Decks.listRoot deckRoot
      -- A set's whole card list is no deck to play.
      let files = Map.fromList [(name, path) | path <- paths, let name = Lobby.deckName deckRoot path, not (Text.isSuffixOf (Text.pack "/all") name)]
      named <- fmap (Map.mapMaybe id) . traverse (fmap (DeckList.formatOf . Encoding.decodeUtf8Lenient) . ByteString.readFile) $ files
      IORef.modifyIORef' (loaded shell) $ \l -> l {deckFiles = Just files, formats = named, unread = Map.toList files}
    readCards = do
      root <- Registry.defaultRoot
      bundles <- Registry.bundlePaths >>= traverse ByteString.readFile
      let byFile = LazyMap.fromList (Registry.loadBundles root bundles)
      _ <- Exception.evaluate (LazyMap.size byFile)
      IORef.modifyIORef' (loaded shell) $ \l -> l {cards = Just byFile}

message :: [String] -> Html.Html msg
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Element.string t] | t <- said]
