-- | The web client as one loop (#9075): the page, the screen it shows, and
-- the data loaded so far, built chunk by chunk (Parun.Web.Loading) one
-- batch at a time while no tap waits.
module Parun.Browser.Shell where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
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
import qualified GHC.Clock as Clock
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Browser.Replay as Replay
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Engine.Quantity as Quantity
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Key as Key
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Key as Key
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.CardIndex as CardIndex
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Loading as Loading
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Replay as Web.Replay
import qualified Parun.Web.Screen as Screen
import qualified System.Environment as Environment
import qualified Text.Read as Read

data Shell = MkShell
  { page :: Browser.Page Screen.Event,
    loaded :: IORef.IORef Loaded,
    -- | How much the page answers for the player (Web.Question.Level), as
    -- the last game left it.
    level :: IORef.IORef Web.Question.Level
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
    registry :: Maybe (Registry.Registry Identity.Identity),
    -- | The manifest's themes (or why it does not decode) and every
    -- scenario's name, once listed.
    scenarioList :: Maybe (Either Text.Text (Map.Map Text.Text [Text.Text]), [Text.Text]),
    -- | The deck builder's card index, once built, and whether the builder
    -- has asked for it.
    cardIndex :: Maybe (Seq.Seq CardIndex.Entry),
    indexWanted :: Bool,
    -- | The index built so far, the cards still to index, and when the
    -- build began, while it runs.
    indexing :: Maybe (Seq.Seq CardIndex.Entry, [Either Text.Text Card.Card], Double),
    -- | Why each saved deck breaks its format, by name, as checked so far.
    illegal :: Map.Map Text.Text [Text.Text],
    -- | The saved decks still to check.
    unchecked :: [(Text.Text, FilePath)],
    -- | The legality table, once a check has loaded it (or why it did not).
    legalityTable :: Maybe (Either Text.Text LegalityTable.LegalityTable),
    -- | Each set's whole card list, by the set's code, once the decks are read.
    setLists :: Map.Map Text.Text FilePath
  }

-- | Where a screen leads when it ends.
data Next
  = MkToHome
  | MkToLobby
  | -- | The builder, on this deck if any.
    MkToBuilder (Maybe Text.Text)
  | MkToReplays
  | -- | This scenario's replay, at this position.
    MkToReplay Text.Text Natural
  | -- | A game played on from this position of this scenario.
    MkToPlayOn Text.Text Natural

-- | What woke a screen's loop.
data Woke
  = MkTapped Screen.Event Text.Text
  | -- | The data changed; the screen redraws.
    MkChanged

-- | The page mounted, the home screen drawn at once, and the loop run; an
-- exception that escapes is shown on the page.
start :: IO ()
start = do
  chosen <- maybe Web.Question.MkStandard (Maybe.fromMaybe Web.Question.MkStandard . Web.Question.levelNamed) <$> Environment.lookupEnv "parun_level"
  shell <- MkShell <$> Browser.mount <*> IORef.newIORef (MkLoaded Loading.new Nothing Map.empty Nothing Map.empty [] Nothing Nothing Nothing False Nothing Map.empty [] Nothing Map.empty) <*> IORef.newIORef chosen
  Exception.catch (opening >>= follow shell) $ \problem ->
    Browser.display (page shell) $ message ["The page stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

-- | Where the page's link leads, read once (engine.js passes its query on):
-- a scenario's replay (parun_scenario, parun_at), played on from there
-- (parun_branch), or the list of them for the empty name; else the home
-- screen.
opening :: IO Next
opening = do
  named <- Environment.lookupEnv "parun_scenario"
  at <- Maybe.fromMaybe 0 . (>>= Read.readMaybe) <$> Environment.lookupEnv "parun_at"
  branching <- Maybe.isJust <$> Environment.lookupEnv "parun_branch"
  pure $ case named of
    Just "" -> MkToReplays
    Just name -> (if branching then MkToPlayOn else MkToReplay) (Text.pack name) at
    Nothing -> MkToHome

-- | Goes where a screen led, and on from there.
follow :: Shell -> Next -> IO ()
follow shell next = case next of
  MkToHome -> home shell Home.new >>= follow shell . afterHome
  MkToLobby -> lobby shell >>= follow shell
  MkToBuilder deck -> builder shell deck >>= follow shell
  MkToReplays -> replays shell >>= follow shell
  MkToReplay name at -> replay shell name at >>= follow shell
  MkToPlayOn name at -> playOnFrom shell name at >>= follow shell

-- | Where the home screen leads.
afterHome :: Home.Outcome -> Next
afterHome outcome = case outcome of
  Home.MkGo Home.MkPlayBots -> MkToLobby
  Home.MkGo Home.MkReplays -> MkToReplays
  Home.MkGo (Home.MkPlayOn name at) -> MkToPlayOn name at
  _ -> MkToHome

-- | The home screen, from this one, until it leads somewhere.
home :: Shell -> Home.Home -> IO Home.Outcome
home shell = go
  where
    go shown = do
      now <- IORef.readIORef (loaded shell)
      let current = shown {Home.problem = Loading.problem (loading now)}
      Browser.display (page shell) (fmap Screen.MkHome (Home.view current))
      woke <- await shell
      case woke of
        MkTapped (Screen.MkHome event) _ -> case Home.update event current of
          Home.MkStay next -> go next
          outcome -> pure outcome
        _ -> go current

-- | The lobby until it goes home or starts a game, filling in as decks
-- and cards arrive; where the game, played from here, then leads.
lobby :: Shell -> IO Next
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
          Lobby.MkHome -> pure MkToHome
          Lobby.MkOpenBuilder -> pure (MkToBuilder Nothing)
          Lobby.MkEdit name -> pure (MkToBuilder (Just name))
          Lobby.MkStart | Right mine <- Lobby.startable current -> do
            others <- traverse (maybe (randomOf (Lobby.opponents current)) (pure . Just)) (Lobby.theirs current)
            files <- Maybe.fromMaybe Map.empty <$> decksRead shell
            -- A bot with no deck to play keeps the lobby up.
            case sequence others >>= traverse (\name -> (,) name <$> Map.lookup name files) . (mine NonEmpty.:|) of
              Just seats -> play shell seats
              Nothing -> go current
          Lobby.MkSubmitImport | Just panel <- Lobby.importing current -> importDeck shell current panel >>= go
          Lobby.MkDelete name | Lobby.isSaved name -> do
            Browser.forgetDeck (Text.drop (Text.length Lobby.savedPrefix) name)
            IORef.modifyIORef' (loaded shell) $ \l -> l {deckFiles = fmap (Map.delete name) (deckFiles l), formats = Map.delete name (formats l), palette = Map.delete name (palette l), unread = filter ((/= name) . fst) (unread l), illegal = Map.delete name (illegal l), unchecked = filter ((/= name) . fst) (unchecked l)}
            go (Lobby.deleted name current)
          Lobby.MkExport name -> do
            files <- Maybe.fromMaybe Map.empty . deckFiles <$> IORef.readIORef (loaded shell)
            contents <- maybe (pure Text.empty) (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) (Map.lookup name files)
            go current {Lobby.exporting = Just (name, contents)}
          Lobby.MkRandom -> do
            picked <- randomOf (if Lobby.choosing current == Lobby.MkMine then Lobby.decks current else Lobby.opponents current)
            go (maybe current (\name -> Lobby.update (Lobby.MkPick name) Text.empty current) picked)
          _ -> go (Lobby.update event value current)
        _ -> go current

-- | The lobby once a pasted deck list is imported under the name typed:
-- checked (Command.check) once the registry is built, "Checking" shown
-- meanwhile, then saved and made the player's; else the panel says why not.
importDeck :: Shell -> Lobby.Lobby -> Lobby.Import -> IO Lobby.Lobby
importDeck shell current panel = case Lobby.nameProblems (Lobby.decks current) (Lobby.importName panel) of
  problems@(_ : _) -> pure (refuse problems)
  [] -> do
    Browser.display (page shell) . fmap Screen.MkLobby $ Lobby.view current {Lobby.importing = Just panel {Lobby.checking = True, Lobby.importProblems = []}}
    built <- untilBuilt shell [Loading.MkDecks, Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
    -- The name again, against the saved decks read meanwhile.
    listed <- IORef.readIORef (loaded shell)
    let named = Lobby.nameProblems (Seq.fromList (foldMap Map.keys (deckFiles listed))) (Lobby.importName panel)
    problems <- case built of
      Left problem -> pure [problem]
      Right reg
        | Maybe.isNothing (deckFiles listed) -> pure [Text.pack "The decks did not load: reload to try again."]
        | null named -> Command.check reg "pasted" contents
        | otherwise -> pure named
    case problems of
      _ : _ -> pure (refuse problems)
      [] -> do
        deckRoot <- Decks.defaultRoot
        let slug = Lobby.slugOf (Lobby.importName panel)
            name = Lobby.savedPrefix <> slug
            path = deckRoot <> "/" <> Text.unpack name <> ".txt"
        Browser.saveDeck slug contents
        IORef.modifyIORef' (loaded shell) $ \l ->
          l
            { deckFiles = fmap (Map.insert name path) (deckFiles l),
              formats = maybe id (Map.insert name) (DeckList.formatOf contents) (formats l),
              unread = (name, path) : unread l
            }
        now <- IORef.readIORef (loaded shell)
        pure (Lobby.imported name (Lobby.withDecks (Seq.fromList (foldMap Map.keys (deckFiles now))) (formats now) current))
  where
    contents = Lobby.importText panel
    refuse problems = current {Lobby.importing = Just panel {Lobby.importProblems = problems, Lobby.checking = False}}

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
      { Lobby.problem = Loading.problemFor Loading.MkDecks (loading now),
        Lobby.palette = palette now,
        Lobby.illegal = illegal now,
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

-- | The deck builder until it leads back to the lobby, on this deck if any
-- (a saved one to save over, any other as a copy): its cards indexed as they
-- load, its list checked after each change once the registry is built, and
-- saved as a lobby deck.
builder :: Shell -> Maybe Text.Text -> IO Next
builder shell deck = do
  need shell Loading.MkDecks
  need shell Loading.MkCards
  IORef.modifyIORef' (loaded shell) $ \l -> l {indexWanted = True}
  files <- Maybe.fromMaybe Map.empty <$> decksRead shell
  initial <- case deck >>= \name -> (,) name <$> Map.lookup name files of
    Just (name, path) -> do
      contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
      pure (Builder.open (if Lobby.isSaved name then Just name else Nothing) (snd (Lobby.describe name)) contents (Builder.new []))
    Nothing -> pure (Builder.new [])
  go initial Nothing
  where
    -- The builder, and the list and format it was last checked at.
    go shown lastChecked = do
      now <- IORef.readIORef (loaded shell)
      named <- if null (Builder.formats shown) then Legality.shippedFormats else pure (Builder.formats shown)
      least <- maybe (Legality.minimumFor (Builder.format shown)) (pure . Just) (Builder.least shown)
      let filled = shown {Builder.index = cardIndex now, Builder.formats = named, Builder.least = least, Builder.sets = Map.keys (setLists now)}
          key = (Builder.deck filled, Builder.format filled, Builder.pool filled)
      -- Taps waiting go first: the list is checked once they are done.
      waiting <- Browser.pending (page shell)
      current <- case registry now of
        Just reg
          | not waiting,
            lastChecked /= Just key,
            Loading.ready Loading.MkRules (loading now) -> do
              (found, cards') <- deckProblems shell reg (heldBy (Builder.pool filled)) (Builder.listText filled)
              pure (Builder.checked found cards' filled, Just key)
        _ -> pure (filled, lastChecked)
      Browser.display (page shell) (fmap Screen.MkBuilder (Builder.view (fst current)))
      woke <- await shell
      let shownNow = fst current
      case woke of
        MkTapped (Screen.MkBuilder event) value -> case event of
          Builder.MkBack | not (Builder.dirty shownNow) -> pure MkToLobby
          Builder.MkLeave -> pure MkToLobby
          Builder.MkSave -> do
            saved' <- saveBuilt shell shownNow
            go saved' (snd current)
          Builder.MkFormat -> go (Builder.update event value shownNow) {Builder.least = Nothing} (snd current)
          Builder.MkPoolChoice
            | Just code <- Text.stripPrefix (Text.pack "set:") value -> do
                listed <- setLists <$> IORef.readIORef (loaded shell)
                names <- maybe (pure []) (fmap (fmap snd . Lobby.entries . Encoding.decodeUtf8Lenient) . ByteString.readFile) (Map.lookup code listed)
                go (Builder.setPool code names shownNow) (snd current)
          _ -> go (Builder.update event value shownNow) (snd current)
        _ -> go shownNow (snd current)

-- | The builder once its deck is saved under the name it was saved as, or
-- the name typed (Builder.saveTarget): written to the deck root and the
-- page, listed in the lobby under the format it now names, recoloured, and
-- queued to be checked afresh; refused while the saved decks it may clash
-- with are unread.
saveBuilt :: Shell -> Builder.Builder -> IO Builder.Builder
saveBuilt shell built = do
  listed <- IORef.readIORef (loaded shell)
  case maybe (Left [Text.pack "The decks did not load: reload to try again."]) (\files -> Builder.saveTarget (Seq.fromList (Map.keys files)) built) (deckFiles listed) of
    Left problems -> pure built {Builder.saveProblems = problems}
    Right name -> do
      deckRoot <- Decks.defaultRoot
      let contents = Builder.listText built
          path = deckRoot <> "/" <> Text.unpack name <> ".txt"
      Browser.saveDeck (Text.drop (Text.length Lobby.savedPrefix) name) contents
      IORef.modifyIORef' (loaded shell) $ \l ->
        l
          { deckFiles = fmap (Map.insert name path) (deckFiles l),
            formats = Map.alter (const (DeckList.formatOf contents)) name (formats l),
            palette = Map.delete name (palette l),
            unread = (name, path) : filter ((/= name) . fst) (unread l),
            illegal = Map.delete name (illegal l),
            unchecked = (name, path) : filter ((/= name) . fst) (unchecked l)
          }
      pure (Builder.saved name built)

-- | A limited pool's cards, for the check (Legality.check's CardPool).
heldBy :: Builder.Pool -> Maybe Legality.CardPool
heldBy chosen = case chosen of
  Builder.MkCardPool held -> Just held
  _ -> Nothing

-- | Why a deck list breaks the format its header names, against the card
-- pool given, as lines, and the cards those lines are about
-- (Legality.namedCard).
-- The legality table is loaded once, at the first check, and kept.
deckProblems :: Shell -> Registry.Registry Identity.Identity -> Maybe Legality.CardPool -> Text.Text -> IO ([Text.Text], Set.Set CardName.CardName)
deckProblems shell reg held contents = case Command.loadText reg "deck" contents of
  Left problems -> pure (problems, Set.empty)
  Right listed -> do
    kept <- legalityTable <$> IORef.readIORef (loaded shell)
    table <- case kept of
      Just t -> pure t
      Nothing -> do
        t <- Legality.tablePath >>= Legality.loadTable
        IORef.modifyIORef' (loaded shell) $ \l -> l {legalityTable = Just t}
        pure t
    let lookUp = either (const . pure . Left) Legality.namedIn table
    (_, lines', violations) <- Legality.assess lookUp Nothing held listed
    pure (lines' <> fmap Legality.explain violations, Set.fromList (Maybe.mapMaybe Legality.namedCard violations))

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
            | problem : _ <- Maybe.mapMaybe (`Loading.problemFor` now) needs -> pure (Left problem)
            | otherwise -> do
                progressed <- awaitWork shell
                Monad.unless progressed Browser.idle
                go
  go

-- | A game between these decks, once the registry is built, shown while it
-- builds; its ending, and the games its Rematch starts;
-- then where it leads. Home from the game shows the home screen with the
-- game kept; a new game chosen there gives it up.
play :: Shell -> NonEmpty.NonEmpty (Text.Text, FilePath) -> IO Next
play shell seats = do
  Browser.display (page shell) (Hotseat.starting (fmap fst seats))
  built <- untilBuilt shell [Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
  case built of
    Left problem -> notice shell [problem]
    Right reg -> do
      chosen <- IORef.newIORef MkToHome
      ended <- Hotseat.game (page shell) reg (level shell) (fmap snd seats) (leaveFor shell chosen)
      case ended of
        Left problems -> notice shell problems
        Right Hotseat.MkAbandoned -> IORef.readIORef chosen
        Right (Hotseat.MkFinished ending) -> do
          let go = do
                Browser.display (page shell) (fmap Screen.MkEnding (Ending.view ending))
                woke <- await shell
                case woke of
                  MkTapped (Screen.MkEnding Ending.MkRematch) _ -> play shell seats
                  MkTapped (Screen.MkEnding Ending.MkNewGame) _ -> pure MkToHome
                  _ -> go
          go

-- | The home screen with a game paused, the replays reachable from it,
-- until the player resumes the game or gives it up for another, recorded
-- in `chosen`.
leaveFor :: Shell -> IORef.IORef Next -> IO Hotseat.Leave
leaveFor shell chosen = go Home.new {Home.paused = True}
  where
    go shown = do
      outcome <- home shell shown
      case outcome of
        Home.MkGo Home.MkReplays -> do
          next <- replays shell
          case next of
            MkToPlayOn name at -> go Home.new {Home.paused = True, Home.confirming = Just (Home.MkPlayOn name at)}
            _ -> go Home.new {Home.paused = True}
        Home.MkGo _ -> IORef.writeIORef chosen (afterHome outcome) >> pure Hotseat.MkAbandon
        _ -> pure Hotseat.MkResume

-- | The list of scenarios, fetched first, until one is opened and left for
-- a game or the home screen.
replays :: Shell -> IO Next
replays shell = do
  need shell Loading.MkScenarios
  let go typed = do
        now <- IORef.readIORef (loaded shell)
        listed <- case scenarioList now of
          Just known -> pure (Just known)
          Nothing
            | Just problem <- Loading.problemFor Loading.MkScenarios (loading now) -> pure (Just (Left problem, []))
            | Loading.ready Loading.MkScenarios (loading now) -> do
                known <- Replay.listing
                IORef.modifyIORef' (loaded shell) $ \l -> l {scenarioList = Just known}
                pure (Just known)
            | otherwise -> pure Nothing
        case listed of
          Just (Left problem, []) -> notice shell [problem]
          Just (Left problem, _) -> notice shell [Text.pack "The scenario manifest does not decode:", problem]
          _ -> do
            Browser.display (page shell) . fmap Screen.MkReplay $ Web.Replay.picker typed (fmap (Bifunctor.first (Either.fromRight Map.empty)) listed)
            woke <- await shell
            case woke of
              MkTapped (Screen.MkReplay Web.Replay.MkSearch) value -> go value
              MkTapped (Screen.MkReplay (Web.Replay.MkOpen name)) _ -> do
                next <- replay shell name 0
                case next of
                  MkToReplays -> go typed
                  _ -> pure next
              MkTapped (Screen.MkReplay Web.Replay.MkHome) _ -> pure MkToHome
              _ -> go typed
  go Text.empty

-- | The scenario, once it and the cards have loaded, replayed from this
-- position until the page leaves it.
replay :: Shell -> Text.Text -> Natural -> IO Next
replay shell name at = opened shell name $ \reg scenario -> do
  exit <- Replay.viewer (page shell) reg name scenario at
  pure $ case exit of
    Replay.MkToList -> MkToReplays
    Replay.MkPlayOn from -> MkToPlayOn name from

-- | A game played on from a position of the scenario, then how it ended,
-- with the way back to the position and to play on again; given up from
-- the home screen for another game, that one.
playOnFrom :: Shell -> Text.Text -> Natural -> IO Next
playOnFrom shell name at = opened shell name $ \reg scenario -> do
  chosen <- IORef.newIORef MkToHome
  ended <- Replay.playOn (page shell) reg (level shell) scenario at (leaveFor shell chosen)
  case ended of
    Nothing -> IORef.readIORef chosen
    Just said -> do
      let go = do
            Browser.display (page shell) . fmap Screen.MkReplay $ Web.Replay.ended said
            woke <- await shell
            case woke of
              MkTapped (Screen.MkReplay Web.Replay.MkResumeReplay) _ -> pure (MkToReplay name at)
              MkTapped (Screen.MkReplay Web.Replay.MkBranch) _ -> pure (MkToPlayOn name at)
              _ -> go
      go

-- | The registry and the scenario named, handed on once loaded, a message
-- shown meanwhile; else why either did not load.
opened :: Shell -> Text.Text -> (Registry.Registry Identity.Identity -> Scenario.Type.Scenario -> IO Next) -> IO Next
opened shell name continue = do
  Browser.display (page shell) (message ["Loading the cards\x2026"])
  built <- untilBuilt shell [Loading.MkCards, Loading.MkRules, Loading.MkScenarios] $ do
    now <- IORef.readIORef (loaded shell)
    pure (if Loading.ready Loading.MkScenarios (loading now) then registry now else Nothing)
  case built of
    Left problem -> notice shell [problem]
    Right reg -> do
      decoded <- Replay.decode name
      either (\problem -> notice shell [Text.pack ("The scenario " <> Text.unpack name <> " does not decode:"), Text.pack problem]) (continue reg) decoded

-- | Why a game could not be played, until the player goes home.
notice :: Shell -> [Text.Text] -> IO Next
notice shell said = do
  Browser.display (page shell) . Element.el "main" ["message"] [] $
    [Element.el "p" [] [] [Element.text t] | t <- said] <> [Element.button ["primary"] (Screen.MkEnding Ending.MkNewGame) [Element.string "Home"]]
  let go = do
        woke <- await shell
        case woke of
          MkTapped (Screen.MkEnding Ending.MkNewGame) _ -> pure MkToHome
          _ -> go
  go

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
      -- The builder's index, 500 cards a batch, timed to the console.
      buildIndex = case (cards now, indexing now) of
        (Just byFile, Nothing)
          | indexWanted now,
            Maybe.isNothing (cardIndex now) -> Just $ do
              began <- Clock.getMonotonicTime
              IORef.modifyIORef' (loaded shell) $ \l -> l {indexing = Just (Seq.empty, LazyMap.elems byFile, began)}
        (_, Just (built, pending, began)) -> Just $ do
          let (batch, rest) = splitAt 500 pending
              entries = Seq.fromList (Maybe.mapMaybe (either (const Nothing) CardIndex.entryOf) batch)
          _ <- Exception.evaluate (foldr (\e n -> Text.length (CardIndex.folded e) + n) 0 entries)
          if null rest
            then do
              ended <- Clock.getMonotonicTime
              let done = built <> entries
              Browser.logLine (Text.pack ("Indexed " <> show (Seq.length done) <> " cards in " <> show (ended - began) <> " s"))
              IORef.modifyIORef' (loaded shell) $ \l -> l {indexing = Nothing, cardIndex = Just done}
            else IORef.modifyIORef' (loaded shell) $ \l -> l {indexing = Just (built <> entries, rest, began)}
        _ -> Nothing
      -- One saved deck checked against its format a batch.
      checkSaved = case (registry now, unchecked now) of
        (Just reg, _ : _) | ready Loading.MkRules -> Just $ do
          let (batch, rest) = splitAt 1 (unchecked now)
          found <- Monad.forM batch $ \(name, path) -> do
            contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
            (,) name . fst <$> deckProblems shell reg Nothing contents
          IORef.modifyIORef' (loaded shell) $ \l -> l {illegal = Map.union (Map.filter (not . null) (Map.fromList found)) (foldr (Map.delete . fst) (illegal l) batch), unchecked = rest}
        _ -> Nothing
      steps =
        [ Monad.guard (ready Loading.MkDecks && Maybe.isNothing (deckFiles now)) >> Just readDecks,
          Monad.guard (ready Loading.MkCards && Maybe.isNothing (cards now)) >> Just readCards
        ]
          <> (if gameWaits then [buildRegistry, readColours] else [readColours, buildRegistry])
          <> [buildIndex, checkSaved]
  case Maybe.catMaybes steps of
    step : _ -> step >> pure True
    [] -> pure False
  where
    readDecks = do
      deckRoot <- Decks.defaultRoot
      paths <- Decks.listRoot deckRoot
      let files = Map.fromList [(name, path) | path <- paths, let name = Lobby.deckName deckRoot path, Lobby.playable name]
      named <- fmap (Map.mapMaybe id) . traverse (fmap (DeckList.formatOf . Encoding.decodeUtf8Lenient) . ByteString.readFile) $ files
      let lists = Map.fromList [(code, path) | path <- paths, Just code <- [Text.stripPrefix (Text.pack "sets/") (Lobby.deckName deckRoot path) >>= Text.stripSuffix (Text.pack "/all")]]
      IORef.modifyIORef' (loaded shell) $ \l -> l {deckFiles = Just files, formats = named, unread = Map.toList files, unchecked = filter (Lobby.isSaved . fst) (Map.toList files), setLists = lists}
    readCards = do
      root <- Registry.defaultRoot
      bundles <- Registry.bundlePaths >>= traverse ByteString.readFile
      let byFile = LazyMap.fromList (Registry.loadBundles root bundles)
      _ <- Exception.evaluate (LazyMap.size byFile)
      IORef.modifyIORef' (loaded shell) $ \l -> l {cards = Just byFile}

message :: [String] -> Html.Html msg
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Element.string t] | t <- said]
