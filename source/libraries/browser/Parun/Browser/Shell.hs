-- | The web client as one loop (#9075): the page, the screen it shows, and
-- the data loaded so far, built chunk by chunk (Parun.Web.Loading) one
-- batch at a time while no tap waits.
module Parun.Browser.Shell where

import Control.Applicative ((<|>))
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.List as List
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
import qualified Parun.Browser.Guest as Guest
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Browser.Replay as Replay
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Engine.Card as EngineCard
import qualified Parun.Engine.Quantity as Quantity
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Key as Key
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Key as Key
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.CardIndex as CardIndex
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Decks as Web.Decks
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.History as History
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Join as Join
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Loading as Loading
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Rejoin as Rejoin
import qualified Parun.Web.Replay as Web.Replay
import qualified Parun.Web.Route as Route
import qualified Parun.Web.Screen as Screen
import qualified System.Environment as Environment

data Shell = MkShell
  { page :: Browser.Page Screen.Event,
    loaded :: IORef.IORef Loaded,
    -- | How much the page answers for the player (Web.Question.Level), as
    -- the last game left it.
    level :: IORef.IORef Web.Question.Level,
    -- | The build the page runs, which a page it links to must share
    -- (Parun.Web.Link).
    build :: Text.Text,
    -- | The games finished in this browser, newest first (Parun.Web.History).
    finished :: IORef.IORef [History.Game],
    -- | How every bot plays, as the lobby last left it: its difficulty and
    -- its persona (Parun.Play.Opponent).
    opponent :: IORef.IORef (Difficulty.Difficulty, Persona.Persona),
    -- | Your deck as the lobby last left it, which a lobby opened on no deck
    -- holds.
    yourDeck :: IORef.IORef (Maybe Text.Text)
  }

-- | The data so far: which chunks arrived, and what has been built from
-- them, each absent until built.
data Loaded = MkLoaded
  { loading :: Loading.Loading,
    -- | Each deck's file, by its name (Lobby.deckName).
    deckFiles :: Maybe (Map.Map Text.Text FilePath),
    -- | The format each deck's header names, by name.
    formats :: Map.Map Text.Text Text.Text,
    -- | The name each deck's header gives it (DeckList.nameOf), by name.
    titles :: Map.Map Text.Text Text.Text,
    -- | Every card file, parsed only when looked up.
    cards :: Maybe (LazyMap.Map FilePath (Either Text.Text Card.Card)),
    -- | Each deck's colours, by name, as read so far.
    palette :: Map.Map Text.Text (Set.Set Color.Color),
    -- | The decks whose colours are still to read.
    unread :: [(Text.Text, FilePath)],
    registry :: Maybe (Registry.Registry Identity.Identity),
    -- | The manifest's themes (or why it does not decode), the description
    -- of each scenario it names, and every scenario's name, once listed.
    scenarioList :: Maybe (Either Text.Text (Map.Map Text.Text [Text.Text]), Map.Map Text.Text Text.Text, [Text.Text]),
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
    setLists :: Map.Map Text.Text FilePath,
    -- | How many cards each deck's main deck holds, by name, as read so far.
    deckSizes :: Map.Map Text.Text Natural,
    -- | Each deck's spells by mana value (DeckBrowser.curveOf), by name, as read
    -- so far.
    deckCurves :: Map.Map Text.Text [Natural],
    -- | Each set's name and release, by its code, once the decks are read.
    releases :: Map.Map Text.Text DeckBrowser.Release
  }

-- | Where a screen leads when it ends.
data Next
  = MkToHome
  | -- | The lobby, this deck yours if any.
    MkToLobby (Maybe Text.Text)
  | -- | The Decks screen, this deck selected if any.
    MkToDecks (Maybe Text.Text)
  | -- | The lobby on these decks: yours, then each bot's.
    MkToMatchup (NonEmpty.NonEmpty Text.Text)
  | -- | The builder, on this deck if any, its Back leading here.
    MkToBuilder (Maybe Text.Text) Next
  | MkToReplays
  | -- | This replay, at this position.
    MkToReplay Web.Replay.Source Natural
  | -- | A game played on from this position of this replay.
    MkToPlayOn Web.Replay.Source Natural
  | -- | Another browser's game, joined with this code from its link.
    MkToJoin Text.Text
  | -- | The game with guests the page kept, picked up where it stopped.
    MkToRestore Link.Saved
  | -- | The lobby as left when the game in progress was conceded for this
    -- event of it, done next.
    MkToConceded Lobby.Lobby Lobby.Event
  | -- | The game in progress, resumed; the home screen where none is.
    MkToPaused

-- | What woke a screen's loop.
data Woke
  = MkTapped Screen.Event Text.Text
  | -- | The data changed; the screen redraws.
    MkChanged
  | -- | A data channel to another browser said something.
    MkHeard Browser.Linked

-- | The page mounted, the home screen drawn at once, and the loop run; an
-- exception that escapes is shown on the page.
start :: IO ()
start = do
  chosen <- maybe Web.Question.MkStandard (Maybe.fromMaybe Web.Question.MkStandard . Web.Question.levelNamed) <$> Environment.lookupEnv "parun_level"
  built <- maybe (Text.pack "unknown") Text.pack <$> Environment.lookupEnv "parun_build"
  -- A summary that does not read, or a game past the cap, is forgotten.
  (games, gone) <- History.loaded <$> Browser.playedGames
  mapM_ Browser.forgetPlayed gone
  shell <- MkShell <$> Browser.mount <*> IORef.newIORef (MkLoaded Loading.new Nothing Map.empty Map.empty Nothing Map.empty [] Nothing Nothing Nothing False Nothing Map.empty [] Nothing Map.empty Map.empty Map.empty Map.empty) <*> IORef.newIORef chosen <*> pure built <*> IORef.newIORef games <*> IORef.newIORef (Lobby.difficulty Lobby.new, Lobby.persona Lobby.new) <*> IORef.newIORef Nothing
  Exception.catch (opening built >>= \next -> Browser.locate Browser.MkReplace (Route.render (routeOf next)) >> follow shell next) $ \problem ->
    Browser.display (page shell) $ message ["The page stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

-- | Where the page's link leads, read once (engine.js passes its query on):
-- another browser's game to join (parun_offer, the link's fragment); a
-- replay or the list of them, as the page's query names (parun_query,
-- Parun.Web.Route); else the game with guests the page kept, played on this
-- build; else the screen the query names. A kept game that does not read is
-- forgotten.
opening :: Text.Text -> IO Next
opening build' = do
  offered <- Environment.lookupEnv "parun_offer"
  route <- maybe Route.MkHome (Route.parse . Text.pack) <$> Environment.lookupEnv "parun_query"
  kept <- Browser.keptGame
  case (offered, route) of
    (Just code, _) | not (null code) -> pure (MkToJoin (Text.pack code))
    (_, Route.MkReplays) -> pure MkToReplays
    (_, Route.MkReplay Route.MkWatching source at) -> pure (MkToReplay source at)
    (_, Route.MkReplay Route.MkPlayingOn source at) -> pure (MkToPlayOn source at)
    _
      | Text.null kept -> pure (nextOf route)
      | otherwise -> either (\problem -> Browser.logLine (Text.pack "The game kept does not read: " <> problem) >> Browser.forgetGame >> pure (nextOf route)) (pure . MkToRestore) (Link.unsaved build' kept)

-- | Where a route leads.
nextOf :: Route.Route -> Next
nextOf route = case route of
  Route.MkHome -> MkToHome
  Route.MkPlay -> MkToLobby Nothing
  Route.MkDecks deck -> MkToDecks deck
  Route.MkBuild deck -> MkToBuilder deck MkToHome
  Route.MkReplays -> MkToReplays
  Route.MkReplay Route.MkWatching source at -> MkToReplay source at
  Route.MkReplay Route.MkPlayingOn source at -> MkToPlayOn source at

-- | The route naming where a screen leads: a game's, the screen it is
-- reloaded into (the lobby, else the home screen, which picks up a game
-- with guests the page kept).
routeOf :: Next -> Route.Route
routeOf next = case next of
  MkToHome -> Route.MkHome
  MkToLobby _ -> Route.MkPlay
  MkToDecks deck -> Route.MkDecks deck
  MkToMatchup _ -> Route.MkPlay
  MkToBuilder deck _ -> Route.MkBuild deck
  MkToReplays -> Route.MkReplays
  MkToReplay source at -> Route.MkReplay Route.MkWatching source at
  MkToPlayOn source at -> Route.MkReplay Route.MkPlayingOn source at
  MkToJoin _ -> Route.MkHome
  MkToRestore _ -> Route.MkHome
  MkToConceded _ _ -> Route.MkPlay
  MkToPaused -> Route.MkHome

-- | Puts where a screen leads in the address bar, a new entry of the
-- browser's history.
located :: Next -> IO ()
located = Browser.locate Browser.MkPush . Route.render . routeOf

-- | Goes where a screen led, and on from there.
follow :: Shell -> Next -> IO ()
follow shell next =
  located next >> case next of
    MkToHome -> home shell Home.new >>= follow shell . afterHome
    MkToPaused -> home shell Home.new >>= follow shell . afterHome
    MkToLobby deck -> lobbyHolding shell deck >>= \shown -> lobby shell shown [] Nothing >>= follow shell
    MkToDecks deck -> decksScreen shell deck >>= follow shell
    MkToMatchup decks -> lobby shell (matchup decks) [] Nothing >>= follow shell
    MkToConceded shown event -> lobby shell shown [] (Just event) >>= follow shell
    MkToBuilder deck back -> builder shell deck back >>= follow shell
    MkToReplays -> replays shell >>= follow shell
    MkToReplay source at -> replay shell source at >>= follow shell
    MkToPlayOn source at -> playOnFrom shell source at >>= follow shell
    MkToJoin code -> joinGame shell code >>= follow shell
    -- Every link of a game picked up is down, so any number serves.
    MkToRestore kept -> play shell (Link.seats kept) (Map.fromList [(i, i) | i <- Link.invited kept]) (Link.locals kept) (Just kept) >>= follow shell

-- | A new lobby with this deck yours, else the deck you chose last, if any.
lobbyHolding :: Shell -> Maybe Text.Text -> IO Lobby.Lobby
lobbyHolding shell deck = do
  last' <- IORef.readIORef (yourDeck shell)
  pure (maybe Lobby.new Lobby.holding (deck <|> last'))

-- | A new lobby on these decks: yours, then each bot's. Only an invited
-- slot reads its deck's list (Lobby.rematch), and none is.
matchup :: NonEmpty.NonEmpty Text.Text -> Lobby.Lobby
matchup decks = Lobby.rematch (fmap (\name -> (name, Text.empty)) decks) Map.empty Map.empty

-- | Where the home screen leads.
afterHome :: Home.Outcome -> Next
afterHome outcome = case outcome of
  Home.MkGo (Home.MkNewGame deck) -> MkToLobby deck
  Home.MkGo Home.MkBuildDeck -> MkToBuilder Nothing MkToHome
  Home.MkGo (Home.MkEditDeck name) -> MkToBuilder (Just name) MkToHome
  Home.MkGo (Home.MkBrowseDecks deck) -> MkToDecks deck
  Home.MkGo Home.MkReplays -> MkToReplays
  Home.MkGo (Home.MkWatch source) -> MkToReplay source 0
  Home.MkGo (Home.MkPlayOn source at) -> MkToPlayOn source at
  Home.MkGo (Home.MkPlayAgain decks) -> MkToMatchup decks
  _ -> MkToHome

-- | Where a section of the top bar leads.
sectionNext :: Bar.Section -> Next
sectionNext section = case section of
  Bar.MkHome -> MkToHome
  Bar.MkPlay -> MkToLobby Nothing
  Bar.MkDecks -> MkToDecks Nothing
  Bar.MkReplays -> MkToReplays

-- | A screen under the top bar, in this section.
framed :: Bar.Section -> Html.Html Screen.Event -> Html.Html Screen.Event
framed section = Bar.framed Screen.MkNav (Just section)

-- | The home screen, from this one, until it leads somewhere, listing the
-- decks saved here as they are read.
home :: Shell -> Home.Home -> IO Home.Outcome
home shell = go
  where
    go shown = do
      now <- IORef.readIORef (loaded shell)
      games <- IORef.readIORef (finished shell)
      let current =
            shown
              { Home.problem = Loading.problem (loading now),
                Home.finished = games,
                Home.saved = [Home.MkDeck name (snd (DeckBrowser.describe (titles now) name)) (Map.findWithDefault Set.empty name (palette now)) (Map.lookup name (deckSizes now)) | name <- foldMap Map.keys (deckFiles now), DeckBrowser.isSaved name]
              }
      Browser.display (page shell) (framed Bar.MkHome (fmap Screen.MkHome (Home.view current)))
      woke <- await shell
      let tapped event = case Home.update event current of
            Home.MkStay next -> go next
            outcome -> pure outcome
      case woke of
        MkTapped (Screen.MkHome event) _ -> tapped event
        MkTapped (Screen.MkNav section) _ -> tapped (Home.navigate section)
        _ -> go current

-- | The lobby, from this one once the invited slots' channels said these
-- and this event, if any, is done, until it goes home or starts a game,
-- filling in as decks and cards arrive, and as the players invited from
-- other browsers connect and choose (Parun.Web.Link), each told the
-- match-up at every change; where the game, played from here, then leads.
-- A lobby with a game paused leads back to it, or concedes it (MkToConceded)
-- before it starts another.
lobby :: Shell -> Lobby.Lobby -> [Browser.Linked] -> Maybe Lobby.Event -> IO Next
lobby shell initial said pending = do
  need shell Loading.MkDecks
  need shell Loading.MkCards
  (difficulty, persona) <- IORef.readIORef (opponent shell)
  heard' <- Monad.foldM heardBy initial {Lobby.difficulty = difficulty, Lobby.persona = persona} said
  go'' heard' Map.empty pending
  where
    go' shown told = go'' shown told Nothing
    go'' shown told first = do
      current <- refreshed shell shown
      IORef.writeIORef (opponent shell) (Lobby.difficulty current, Lobby.persona current)
      IORef.writeIORef (yourDeck shell) (Lobby.mine current)
      told' <- tellPlaces current told
      if Lobby.starts current
        then startWith current told'
        else do
          Browser.display (page shell) (framed Bar.MkPlay (fmap Screen.MkLobby (Lobby.view current)))
          woke <- maybe (await shell) (\event -> pure (MkTapped (Screen.MkLobby event) Text.empty)) first
          -- Every invite the lobby drops has its channel closed.
          let linksOf = Set.fromList . fmap Lobby.linkNumber . Map.elems . Lobby.invites
              go next = do
                mapM_ Browser.linkClose (Set.difference (linksOf current) (linksOf next))
                go' next (Map.restrictKeys told' (linksOf next))
              leaving next = mapM_ Browser.linkClose (linksOf current) >> pure next
          case woke of
            MkHeard heard' -> heardBy current heard' >>= go
            MkTapped (Screen.MkNav section) _
              | section /= Bar.MkPlay -> leaving (sectionNext section)
            MkTapped (Screen.MkLobby event) value -> case event of
              Lobby.MkOpenBuilder -> leaving (MkToBuilder Nothing (MkToLobby Nothing))
              Lobby.MkEdit name -> leaving (MkToBuilder (Just name) (MkToLobby Nothing))
              Lobby.MkConcede | Just (left, event') <- Lobby.conceded current -> leaving (MkToConceded left event')
              Lobby.MkResume -> leaving MkToPaused
              Lobby.MkStart
                | not (Lobby.paused current),
                  Map.null (Lobby.invites current),
                  Right mine <- Lobby.startable current -> do
                    others <- dealtFor shell current
                    files <- Maybe.fromMaybe Map.empty <$> decksRead shell
                    -- A bot with no deck to play keeps the lobby up.
                    case sequence others >>= traverse (\name -> (,) name <$> Map.lookup name files) . (mine NonEmpty.:|) of
                      Just seats -> listsOf seats >>= \lists -> play shell lists Map.empty (Lobby.locals current) Nothing
                      Nothing -> go current
              Lobby.MkAdd Lobby.MkInvited | not (Lobby.paused current), length (Lobby.theirs current) < Lobby.mostOthers -> Browser.linkHost (Lobby.nextLink current) >> go (Lobby.update event value current)
              Lobby.MkConnect i | Just invite <- Map.lookup i (Lobby.invites current) -> do
                Browser.linkAccept (Lobby.linkNumber invite) (Text.strip (Lobby.replyCode invite))
                go current
              Lobby.MkBrowse DeckBrowser.MkSubmitImport -> do
                (browsed, done) <- importDeck shell (\shown' -> Browser.display (page shell) . framed Bar.MkPlay . fmap Screen.MkLobby $ Lobby.view current {Lobby.browser = shown'}) (Lobby.browser current)
                go (maybe id Lobby.imported done current {Lobby.browser = browsed})
              Lobby.MkBrowse (DeckBrowser.MkDelete name) | DeckBrowser.isSaved name -> do
                forgetSaved shell name
                go (Lobby.deleted name current)
              Lobby.MkBrowse (DeckBrowser.MkExport name) -> do
                contents <- Maybe.fromMaybe Text.empty <$> deckText shell name
                go (Lobby.browsed (\b -> b {DeckBrowser.exporting = Just (name, contents)}) current)
              Lobby.MkRandom -> do
                picked <- randomOf (Seq.fromList (Lobby.shown current))
                go (maybe current (\name -> Lobby.update (Lobby.MkPick name) Text.empty current) picked)
              _ -> go (Lobby.update event value current)
            _ -> go current
    -- What an invited slot's channel said: its link, its opening, a
    -- message, or its closing, which makes a new link to send.
    heardBy current heard' = case Lobby.seatOfLink (Browser.link heard') current of
      Nothing -> pure current
      Just i -> case Text.unpack (Browser.kind heard') of
        "offer" -> pure (Lobby.linked i (\invite -> invite {Lobby.link = Just (Browser.said heard')}) current)
        "open" -> pure (Lobby.linked i (\invite -> invite {Lobby.connected = True, Lobby.inviteProblem = Nothing}) current)
        "message" -> case Link.decode (build shell) (Browser.said heard') of
          Right told -> pure (Lobby.heard i told current)
          Left problem -> reinvite i (capitalised problem <> Text.pack ".") current
        "closed" -> reinvite i connectionClosed current
        _ -> pure current
    -- Each connected player told the match-up where it changed since, by
    -- link number.
    tellPlaces current told =
      fmap Map.fromList . Monad.forM [(Lobby.linkNumber invite, Lobby.placesFor i current) | (i, invite) <- Map.toList (Lobby.invites current), Lobby.connected invite] $ \(n, places) -> do
        Monad.when (Map.lookup n told /= Just places) $ Link.send (Browser.channel (page shell) (build shell) n) (Link.MkMatchup places)
        pure (n, places)
    -- Every player is ready: the game starts, each player invited told
    -- their seat, each bot without a deck given one at random.
    startWith current told = do
      others <- dealtFor shell current
      files <- Maybe.fromMaybe Map.empty <$> decksRead shell
      let invited = Lobby.invites current
          slot (i, name) = case Map.lookup i invited >>= Lobby.guestDeck of
            Just (theirs, contents) -> pure (Just (theirs, contents))
            Nothing -> traverse (\(held, path) -> (,) held . Encoding.decodeUtf8Lenient <$> ByteString.readFile path) (name >>= \held -> (,) held <$> Map.lookup held files)
      mineFile <- traverse (\name -> traverse (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) (Map.lookup name files)) (Lobby.mine current)
      seated <- traverse slot (zip [0 ..] others)
      case (,) <$> Lobby.mine current <*> Monad.join mineFile of
        Just mine | Just rest <- sequence seated -> do
          -- Each player told the match-up again with every bot's deck drawn,
          -- which the history of their game names (Join.places), then their seat.
          Monad.forM_ (Map.toList invited) $ \(i, invite) -> do
            let channel = Browser.channel (page shell) (build shell) (Lobby.linkNumber invite)
            Link.send channel (Link.MkMatchup (Lobby.placesFor i current {Lobby.theirs = others}))
            Link.send channel (Link.MkStart (seatOf i) Ending.unseen)
          play shell (mine NonEmpty.:| rest) (fmap Lobby.linkNumber invited) (Lobby.locals current) Nothing
        -- A bot with no deck to play keeps the lobby up.
        _ -> go' current {Lobby.ready = False} told

-- | The lobby with invited slot `i` made afresh, a new link made for it
-- under its number, saying why.
reinvite :: Int -> Text.Text -> Lobby.Lobby -> IO Lobby.Lobby
reinvite i problem current = case Map.lookup i (Lobby.invites current) of
  Nothing -> pure current
  Just invite -> do
    Browser.linkHost (Lobby.linkNumber invite)
    pure (Lobby.linked i (const (Lobby.newInvite (Lobby.linkNumber invite)) {Lobby.inviteProblem = Just problem}) current)

-- | Why an invited slot was made afresh once its channel closed.
connectionClosed :: Text.Text
connectionClosed = Text.pack "The connection closed: send them the new link."

-- | The seat the player invited to this bot's slot plays.
seatOf :: Int -> PlayerId.PlayerId
seatOf i = PlayerId.MkPlayerId (Int.toNaturalSaturating i + 1)

-- | A problem as a sentence begins.
capitalised :: Text.Text -> Text.Text
capitalised t = maybe t (\(c, rest) -> Text.cons (Char.toUpper c) rest) (Text.uncons t)

-- | Each deck's name and its list's text.
listsOf :: NonEmpty.NonEmpty (Text.Text, FilePath) -> IO (NonEmpty.NonEmpty (Text.Text, Text.Text))
listsOf = traverse (\(name, path) -> (,) name . Encoding.decodeUtf8Lenient <$> ByteString.readFile path)

-- | The browser once its pasted deck list is imported under the name
-- typed, and the deck's name once saved: checked (Command.check) once the
-- registry is built, `showing` the panel "Checking" meanwhile, then saved
-- (rememberSaved); else the panel says why not.
importDeck :: Shell -> (DeckBrowser.DeckBrowser -> IO ()) -> DeckBrowser.DeckBrowser -> IO (DeckBrowser.DeckBrowser, Maybe Text.Text)
importDeck shell showing current = case DeckBrowser.importing current of
  Nothing -> pure (current, Nothing)
  Just panel ->
    let contents = DeckBrowser.importText panel
        refuse problems = (current {DeckBrowser.importing = Just panel {DeckBrowser.importProblems = problems, DeckBrowser.checking = False}}, Nothing)
     in case DeckBrowser.nameProblems (DeckBrowser.decks current) (DeckBrowser.importName panel) of
          problems@(_ : _) -> pure (refuse problems)
          [] -> do
            showing current {DeckBrowser.importing = Just panel {DeckBrowser.checking = True, DeckBrowser.importProblems = []}}
            built <- untilBuilt shell quiet [Loading.MkDecks, Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
            -- The name again, against the saved decks read meanwhile.
            listed <- IORef.readIORef (loaded shell)
            let named = DeckBrowser.nameProblems (Seq.fromList (foldMap Map.keys (deckFiles listed))) (DeckBrowser.importName panel)
            problems <- case built of
              Left problem -> pure [problem]
              Right reg
                | Maybe.isNothing (deckFiles listed) -> pure [Text.pack "The decks did not load: reload to try again."]
                | null named -> Command.check reg contents
                | otherwise -> pure named
            case problems of
              _ : _ -> pure (refuse problems)
              [] -> do
                let name = DeckBrowser.savedPrefix <> DeckBrowser.slugOf (DeckBrowser.importName panel)
                rememberSaved shell name (DeckBrowser.importName panel) contents
                now <- IORef.readIORef (loaded shell)
                pure (DeckBrowser.withDecks (Seq.fromList (foldMap Map.keys (deckFiles now))) (formats now) (titles now) current, Just name)

-- | Saves a deck list as this saved deck, over any of that name, headed by
-- the name typed for it (DeckList.withName): written to the deck root and the
-- page (Browser.saveDeck), listed under the format and name it gives, and
-- queued to be read and checked afresh.
rememberSaved :: Shell -> Text.Text -> Text.Text -> Text.Text -> IO ()
rememberSaved shell name typed listed = do
  let contents = DeckList.withName typed listed
  deckRoot <- Decks.defaultRoot
  let path = deckRoot <> "/" <> Text.unpack name <> ".txt"
  Browser.saveDeck (Text.drop (Text.length DeckBrowser.savedPrefix) name) contents
  IORef.modifyIORef' (loaded shell) $ \l ->
    l
      { deckFiles = fmap (Map.insert name path) (deckFiles l),
        formats = Map.alter (const (DeckList.formatOf contents)) name (formats l),
        titles = Map.alter (const (DeckList.nameOf contents)) name (titles l),
        palette = Map.delete name (palette l),
        deckCurves = Map.delete name (deckCurves l),
        deckSizes = Map.delete name (deckSizes l),
        unread = (name, path) : filter ((/= name) . fst) (unread l),
        illegal = Map.delete name (illegal l),
        unchecked = (name, path) : filter ((/= name) . fst) (unchecked l)
      }

-- | Deletes this saved deck from the page (Browser.forgetDeck) and from all
-- that was read of it.
forgetSaved :: Shell -> Text.Text -> IO ()
forgetSaved shell name = do
  Browser.forgetDeck (Text.drop (Text.length DeckBrowser.savedPrefix) name)
  IORef.modifyIORef' (yourDeck shell) (Monad.mfilter (/= name))
  IORef.modifyIORef' (loaded shell) $ \l -> l {deckFiles = fmap (Map.delete name) (deckFiles l), formats = Map.delete name (formats l), titles = Map.delete name (titles l), palette = Map.delete name (palette l), deckCurves = Map.delete name (deckCurves l), deckSizes = Map.delete name (deckSizes l), unread = filter ((/= name) . fst) (unread l), illegal = Map.delete name (illegal l), unchecked = filter ((/= name) . fst) (unchecked l)}

-- | A deck's list, as its file holds it, if the deck is held.
deckText :: Shell -> Text.Text -> IO (Maybe Text.Text)
deckText shell name = do
  files <- Maybe.fromMaybe Map.empty . deckFiles <$> IORef.readIORef (loaded shell)
  traverse (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) (Map.lookup name files)

-- | The lobby with what has loaded since: the decks once read, and what
-- is read of them (catalogued), the lists of the decks chosen among it.
refreshed :: Shell -> Lobby.Lobby -> IO Lobby.Lobby
refreshed shell shown = do
  now <- IORef.readIORef (loaded shell)
  let withDecks = case deckFiles now of
        Just files | DeckBrowser.reading (Lobby.browser shown) -> Lobby.withDecks (Seq.fromList (Map.keys files)) (formats now) (titles now) shown
        _ -> shown
  browsed <- catalogued shell (Maybe.catMaybes (Lobby.mine withDecks : Lobby.theirs withDecks)) (Lobby.browser withDecks)
  pure withDecks {Lobby.browser = browsed}

-- | A deck browser with what has loaded since: why the decks did not load,
-- their colours, legality, sizes and curves so far, and the lists of the
-- decks `wanted` not yet previewed, once the cards are read.
catalogued :: Shell -> [Text.Text] -> DeckBrowser.DeckBrowser -> IO DeckBrowser.DeckBrowser
catalogued shell wanted browsed = do
  now <- IORef.readIORef (loaded shell)
  games <- IORef.readIORef (finished shell)
  read' <- case (cards now, deckFiles now) of
    (Just byFile, Just files) -> do
      root <- Registry.defaultRoot
      Monad.forM [name | name <- wanted, Map.notMember name (DeckBrowser.previews browsed)] $ \name -> do
        contents <- maybe (pure Text.empty) (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) (Map.lookup name files)
        pure (name, [if section == DeckList.Main then line else line {DeckBrowser.kind = DeckBrowser.MkBeside section} | (section, n, card) <- Lobby.sectioned contents, let line = lineOf root byFile n card])
    _ -> pure []
  pure
    browsed
      { DeckBrowser.problem = Loading.problemFor Loading.MkDecks (loading now),
        DeckBrowser.palette = palette now,
        DeckBrowser.illegal = illegal now,
        DeckBrowser.sizes = deckSizes now,
        DeckBrowser.curves = deckCurves now,
        DeckBrowser.previews = Map.union (DeckBrowser.previews browsed) (Map.fromList read'),
        DeckBrowser.releases = releases now,
        DeckBrowser.played = History.lastPlayed games
      }

-- | A card of a deck's list, as the lobby previews it.
lineOf :: FilePath -> LazyMap.Map FilePath (Either Text.Text Card.Card) -> Natural -> CardName.CardName -> DeckBrowser.Line
lineOf root byFile n name = case LazyMap.lookup (Registry.cardPath root (Registry.slugFor name)) byFile of
  Just (Right c)
    | face <- NonEmpty.head (Card.faces c) ->
        let types = TypeLine.types (Face.typeLine face)
            kind
              | Set.member CardType.Land types = DeckBrowser.MkLand
              | Set.member CardType.Creature types = DeckBrowser.MkCreature
              | otherwise = DeckBrowser.MkSpell
         in -- CR 709.4b: a split card's cost and mana value are its halves'.
            DeckBrowser.MkLine n name (Face.manaCost (EngineCard.combined c)) (Quantity.manaValueOf (EngineCard.combined c)) kind
  _ -> DeckBrowser.MkLine n name Nothing 0 DeckBrowser.MkSpell

-- | The Decks screen, this deck selected if any, until it leads elsewhere,
-- filling in as the decks and cards arrive, the selected deck's list read
-- for its detail: a deck imported, renamed, duplicated or deleted here is
-- saved or forgotten as the builder's are (rememberSaved, forgetSaved).
decksScreen :: Shell -> Maybe Text.Text -> IO Next
decksScreen shell chosen = do
  need shell Loading.MkDecks
  need shell Loading.MkCards
  go (Web.Decks.new chosen)
  where
    display screen = Browser.display (page shell) (framed Bar.MkDecks (fmap Screen.MkDecks (Web.Decks.view screen)))
    go shown = do
      current <- refreshedDecks shown
      display current
      woke <- await shell
      now' <- IORef.readIORef (loaded shell)
      let unread' = Text.pack "The decks did not load: reload to try again."
      case woke of
        MkTapped (Screen.MkNav section) _
          | section /= Bar.MkDecks -> pure (sectionNext section)
        MkTapped (Screen.MkDecks event) value -> case event of
          Web.Decks.MkNewDeck -> pure (MkToBuilder Nothing (MkToDecks (Web.Decks.selected current)))
          Web.Decks.MkEdit name -> pure (MkToBuilder (Just name) (MkToDecks (Just name)))
          Web.Decks.MkPlay name -> pure (MkToLobby (Just name))
          Web.Decks.MkSubmitRename -> case Web.Decks.renameTarget current of
            Left problems -> go (Web.Decks.refused problems current)
            Right _ | Maybe.isNothing (deckFiles now') -> go (Web.Decks.refused [unread'] current)
            Right (old, renamed) -> do
              let typed = maybe Text.empty fst (Web.Decks.renaming current)
              read' <- deckText shell old
              case read' of
                Nothing -> go (Web.Decks.refused [Text.pack "That deck could not be read."] current)
                -- The same file, its name as typed.
                Just contents
                  | old == renamed -> do
                      rememberSaved shell old typed contents
                      go (Web.Decks.update Web.Decks.MkCancelRename Text.empty current)
                  | otherwise -> do
                      rememberSaved shell renamed typed contents
                      forgetSaved shell old
                      go (Web.Decks.savedAs renamed (Web.Decks.deleted old current))
          Web.Decks.MkDuplicate name
            | DeckBrowser.isSaved name,
              Just files <- deckFiles now' -> do
                read' <- deckText shell name
                case read' of
                  Nothing -> go current
                  Just contents -> do
                    let copy = Web.Decks.copyName (Seq.fromList (Map.keys files)) name
                    rememberSaved shell copy (Web.Decks.copyTitle (DeckBrowser.titles (Web.Decks.browser current)) name copy) contents
                    go (Web.Decks.savedAs copy current)
          Web.Decks.MkBrowse DeckBrowser.MkSubmitImport -> do
            (browsed, done) <- importDeck shell (\shown' -> display current {Web.Decks.browser = shown'}) (Web.Decks.browser current)
            go (maybe id Web.Decks.savedAs done current {Web.Decks.browser = browsed})
          Web.Decks.MkBrowse (DeckBrowser.MkDelete name) | DeckBrowser.isSaved name -> do
            forgetSaved shell name
            go (Web.Decks.deleted name current)
          Web.Decks.MkBrowse (DeckBrowser.MkExport name) -> do
            contents <- Maybe.fromMaybe Text.empty <$> deckText shell name
            go (Web.Decks.browsed (\b -> b {DeckBrowser.exporting = Just (name, contents)}) current)
          _ -> go (Web.Decks.update event value current)
        _ -> go current
    -- The decks as read so far, and what is read of them.
    refreshedDecks shown = do
      now <- IORef.readIORef (loaded shell)
      let withDecks = maybe shown (\files -> Web.Decks.withDecks (Seq.fromList (Map.keys files)) (formats now) (titles now) shown) (deckFiles now)
      browsed <- catalogued shell (Maybe.maybeToList (Web.Decks.selected withDecks)) (Web.Decks.browser withDecks)
      pure withDecks {Web.Decks.browser = browsed}

-- | The deck builder until it leads back where `back` says, or elsewhere
-- from the top bar, on this deck if any (a saved one to save over, any
-- other as a copy): its cards indexed as they load, its list checked after
-- each change once the registry is built, and saved as a lobby deck. Leaving
-- with changes unsaved asks first.
builder :: Shell -> Maybe Text.Text -> Next -> IO Next
builder shell deck back = do
  need shell Loading.MkDecks
  need shell Loading.MkCards
  IORef.modifyIORef' (loaded shell) $ \l -> l {indexWanted = True}
  files <- Maybe.fromMaybe Map.empty <$> decksRead shell
  listed <- IORef.readIORef (loaded shell)
  initial <- case deck >>= \name -> (,) name <$> Map.lookup name files of
    Just (name, path) -> do
      contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
      pure (Builder.open (if DeckBrowser.isSaved name then Just name else Nothing) (snd (DeckBrowser.describe (titles listed) name)) contents (Builder.new []))
    Nothing -> pure (Builder.new []) {Builder.notices = [Text.pack "That deck could not be opened, so this is a new one." | Maybe.isJust deck]}
  go initial Nothing back
  where
    -- The builder, the list it was last checked at, and where leaving it
    -- leads.
    go shown lastChecked leaving = do
      now <- IORef.readIORef (loaded shell)
      named <- if null (Builder.formats shown) then Legality.builtFormats else pure (Builder.formats shown)
      least <- maybe (Legality.minimumFor (Builder.format shown)) (pure . Just) (Builder.least shown)
      let indexed = if Maybe.isNothing (Builder.index shown) then maybe id Builder.withIndex (cardIndex now) shown else shown
          filled = indexed {Builder.formats = named, Builder.least = least, Builder.sets = Map.keys (setLists now), Builder.loadProblem = Loading.problem (loading now)}
          -- Checked again whenever the list it saves would change.
          key = Builder.listText filled
      -- Taps waiting go first: the list is checked once they are done.
      waiting <- Browser.pending (page shell)
      current <- case registry now of
        Just reg
          | not waiting,
            lastChecked /= Just key,
            Loading.ready Loading.MkRules (loading now) -> do
              (found, cards') <- deckProblems shell reg (Builder.listText filled)
              pure (Builder.checked found cards' filled, Just key)
        _ -> pure (filled, lastChecked)
      Browser.display (page shell) (framed Bar.MkDecks (fmap Screen.MkBuilder (Builder.view (fst current))))
      woke <- await shell
      let shownNow = fst current
          -- Leaves for `next`, asking first while changes are unsaved.
          leave next
            | Builder.dirty shownNow = go (Builder.update Builder.MkBack Text.empty shownNow) (snd current) next
            | otherwise = pure (returnTo shownNow next)
      case woke of
        MkTapped (Screen.MkNav section) _ -> leave (sectionNext section)
        MkTapped (Screen.MkBuilder event) value -> case event of
          Builder.MkBack -> leave back
          -- Where it leads as the builder now stands: a save since the
          -- question was asked selects the deck saved.
          Builder.MkLeave -> pure (returnTo shownNow leaving)
          Builder.MkSave -> do
            saved' <- saveBuilt shell shownNow
            go saved' (snd current) leaving
          Builder.MkFormat -> go (Builder.update event value shownNow) {Builder.least = Nothing} (snd current) leaving
          Builder.MkPoolChoice
            | Just code <- Text.stripPrefix (Text.pack "set:") value -> do
                listed <- setLists <$> IORef.readIORef (loaded shell)
                read' <- traverse (Exception.try . ByteString.readFile) (Map.lookup code listed)
                go
                  ( case read' of
                      Just (Right bytes) -> Builder.setPool code (fmap snd (Lobby.entries (Encoding.decodeUtf8Lenient bytes))) shownNow
                      Just (Left problem) -> shownNow {Builder.poolProblems = [Text.pack ("The set's cards did not load: " <> Exception.displayException (problem :: Exception.IOException))]}
                      Nothing -> shownNow {Builder.poolProblems = [Text.pack "No such set: " <> code]}
                  )
                  (snd current)
                  leaving
          _ -> go (Builder.update event value shownNow) (snd current) leaving
        _ -> go shownNow (snd current) leaving

-- | Where leaving the builder for `next` leads: the Decks screen on the
-- deck it saved, where it leads there at all.
returnTo :: Builder.Builder -> Next -> Next
returnTo built next = case next of
  MkToDecks selection -> MkToDecks (Web.Decks.returning selection built)
  _ -> next

-- | The builder once its deck is saved under the name it was saved as, or
-- the name typed (Builder.saveTarget), as rememberSaved saves it; refused
-- while the saved decks it may clash with are unread.
saveBuilt :: Shell -> Builder.Builder -> IO Builder.Builder
saveBuilt shell built = do
  listed <- IORef.readIORef (loaded shell)
  case maybe (Left [Text.pack "The decks did not load: reload to try again."]) (\files -> Builder.saveTarget (Seq.fromList (Map.keys files)) built) (deckFiles listed) of
    Left problems -> pure built {Builder.saveProblems = problems}
    Right name -> do
      -- A name left blank keeps the one the deck had.
      let typed = if Text.null (Text.strip (Builder.saveName built)) then snd (DeckBrowser.describe (titles listed) name) else Builder.saveName built
      rememberSaved shell name typed (Builder.listText built)
      pure (Builder.saved name built)

-- | Why a deck list breaks the format its header names, as lines, and the
-- cards those lines are about (Legality.namedCard). No card pool is given:
-- a pooled list writes its pool's other cards as its sideboard (CR 100.4b),
-- so the pool is the list's own, and the builder caps each card at it.
deckProblems :: Shell -> Registry.Registry Identity.Identity -> Text.Text -> IO ([Text.Text], Set.Set CardName.CardName)
deckProblems shell reg contents = case Command.loadText reg "deck" contents of
  Left problems -> pure (fmap (Command.unlabelled "deck") problems, Set.empty)
  Right listed -> do
    table <- tableOf shell
    let lookUp = either (const . pure . Left) Legality.namedIn table
    (_, lines', violations) <- Legality.assess lookUp Nothing Nothing listed
    pure (lines' <> fmap Legality.explain violations, Set.fromList (Maybe.mapMaybe Legality.namedCard violations))

-- | The legality table, loaded at the first ask and kept.
tableOf :: Shell -> IO (Either Text.Text LegalityTable.LegalityTable)
tableOf shell = do
  kept <- legalityTable <$> IORef.readIORef (loaded shell)
  case kept of
    Just t -> pure t
    Nothing -> do
      t <- Legality.tablePath >>= Legality.loadTable
      IORef.modifyIORef' (loaded shell) $ \l -> l {legalityTable = Just t}
      pure t

-- | One of these decks at random.
randomOf :: Seq.Seq Text.Text -> IO (Maybe Text.Text)
randomOf among = do
  key <- Key.fresh
  let roll = foldr (\w acc -> acc * 4294967296 + toInteger w) 0 (Key.unwrap key)
  pure (Integer.toInt (mod roll (toInteger (max 1 (Seq.length among)))) >>= (`Seq.lookup` among))

-- | Each bot's deck, a bot left to a random one dealt from its
-- 'Lobby.opponents' holding at least 'Lobby.fewestDealt' cards against
-- yours.
dealtFor :: Shell -> Lobby.Lobby -> IO [Maybe Text.Text]
dealtFor shell current = do
  yours <- maybe (pure 0) (deckSize shell) (Lobby.mine current)
  let keeps name = (>= Lobby.fewestDealt yours) <$> deckSize shell name
  traverse (maybe (Lobby.dealt randomOf keeps (Lobby.opponents current)) (pure . Just)) (Lobby.theirs current)

-- | How many cards a deck's main deck holds: as read so far, else read now;
-- none for a deck with no file.
deckSize :: Shell -> Text.Text -> IO Natural
deckSize shell name = do
  known <- Map.lookup name . deckSizes <$> IORef.readIORef (loaded shell)
  case known of
    Just size -> pure size
    Nothing -> maybe 0 Lobby.mainCount <$> deckText shell name

-- | The deck files, once read; nothing if the decks failed to load.
decksRead :: Shell -> IO (Maybe (Map.Map Text.Text FilePath))
decksRead shell = either (const Nothing) Just <$> untilBuilt shell quiet [Loading.MkDecks] (deckFiles <$> IORef.readIORef (loaded shell))

-- | Waits, loading meanwhile, until the value is built, taps waiting,
-- showing each change in what has loaded as `waiting` says; or why one of
-- the needs it is built from did not load.
untilBuilt :: Shell -> (Loading.Loading -> IO ()) -> [Loading.Need] -> IO (Maybe a) -> IO (Either Text.Text a)
untilBuilt shell waiting needs built = do
  mapM_ (need shell) needs
  let go before = do
        done <- built
        now <- loading <$> IORef.readIORef (loaded shell)
        case done of
          Just a -> pure (Right a)
          Nothing
            | problem : _ <- Maybe.mapMaybe (`Loading.problemFor` now) needs -> pure (Left problem)
            | otherwise -> do
                Monad.when (before /= Just now) (waiting now)
                progressed <- awaitWork shell
                Monad.unless progressed Browser.idle
                go (Just now)
  go Nothing

-- | Nothing shown while waiting (untilBuilt): the screen already says so.
quiet :: Loading.Loading -> IO ()
quiet = const (pure ())

-- | The loading screen shown while waiting (untilBuilt).
loadingScreen :: Shell -> Loading.Loading -> IO ()
loadingScreen shell = Browser.display (page shell) . Loading.view

-- | A game between these decks, each a name and its list's text, once the
-- registry is built, shown while it builds, every bot playing as the lobby
-- last left it (opponent), each bot's slot in `invited`
-- played from another browser over the link of its number there
-- (Parun.Web.Link) and each
-- in `here` on this screen as it sits there, or that
-- game picked up where `resuming` stopped, its bots playing as it was
-- kept; its ending, and the games its Rematch starts; then where it leads. A game with guests is kept until it
-- is over (Link.saved), and a guest whose connection drops, or every guest
-- of a game picked up, is asked to rejoin (rejoin) when next asked. Home
-- from the game shows the home screen with the game kept; a new game chosen
-- there gives it up. Every player invited is told how it ended; a Rematch
-- with them goes back to the lobby on the match-up played (Lobby.rematch),
-- their channels kept open and what they said meanwhile heard there, and a
-- new game closes them.
play :: Shell -> NonEmpty.NonEmpty (Text.Text, Text.Text) -> Map.Map Int Int -> Map.Map Int Link.Sitting -> Maybe Link.Saved -> IO Next
play shell seats invited here resuming = do
  let starting = Browser.display (page shell) (if Maybe.isJust resuming then message ["Picking the game up where it stopped\x2026"] else Hotseat.starting seats)
  starting
  built <- untilBuilt shell (loadingScreen shell) [Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
  -- The invited slots whose connection is down: every one of a game picked up.
  down <- IORef.newIORef (if Maybe.isJust resuming then Map.keysSet invited else Set.empty)
  let linkOf i = Map.findWithDefault i i invited
      channelOf i = tracked down i (Browser.channel (page shell) (build shell) (linkOf i))
      tellAll endingFor = Monad.forM_ (Map.keys invited) $ \i -> Link.send (channelOf i) (Link.MkEnd (endingFor (seatOf i)))
      closeAll = mapM_ Browser.linkClose invited
      stopped said = tellAll (\seat -> Ending.endingOf Ending.MkStopped said Text.empty seat Ending.unseen) >> closeAll
      keeping = not (Map.null invited)
  case built of
    Left problem -> stopped [problem] >> notice shell [problem]
    Right reg -> do
      -- Again, should the loading screen have replaced it meanwhile.
      starting
      chosen <- IORef.newIORef MkToHome
      seed <- maybe Key.fresh (pure . Link.seed) resuming
      -- A game picked up plays its bots as it was kept, and the lobby a
      -- Rematch leads to offers that.
      Monad.forM_ resuming $ \game -> IORef.writeIORef (opponent shell) (Link.difficulty game, Link.persona game)
      bots@(difficulty, persona) <- IORef.readIORef (opponent shell)
      Monad.when (keeping && Maybe.isNothing resuming) . Browser.keepGame $ Link.saved (build shell) (Link.MkSaved seed seats (Map.keys invited) here difficulty persona [])
      let rejoinSlot i = do
            back <- rejoin shell (linkOf i) (Rejoin.host (Text.pack ("Player " <> show (i + 2))))
            Monad.when back (IORef.modifyIORef' down (Set.delete i))
            pure back
          -- A rejoining guest is told the match-up played, every bot's deck drawn.
          remotes = Map.fromList [(seatOf i, \seen -> Link.remote (rejoinSlot i) (Lobby.placesFor i (Lobby.rematch seats invited here)) seen (channelOf i)) | i <- Map.keys invited]
          kept = if keeping then Browser.keepReply . Link.savedReply else const (pure ())
      -- Each guest is told whose decision the game waits on (Link.telling).
      told <- IORef.newIORef Map.empty
      let decides seat = IORef.readIORef told >>= Link.telling (Map.fromList [(seatOf i, channelOf i) | i <- Map.keys invited]) seat >>= IORef.writeIORef told
      parsed <- foldMap LazyMap.elems . cards <$> IORef.readIORef (loaded shell)
      ended <- Hotseat.game (page shell) reg parsed (tableOf shell) (level shell) bots (fmap (Bifunctor.first Text.unpack) seats) remotes (Map.mapKeys seatOf here) seed (foldMap Link.replies resuming) kept decides tellAll (leaveFor shell chosen)
      Monad.when keeping Browser.forgetGame
      case ended of
        Left problems -> stopped problems >> notice shell problems
        Right Ending.MkAbandoned -> stopped [Text.pack "The host gave the game up."] >> IORef.readIORef chosen
        Right played@(Ending.MkFinished ending) -> do
          palette' <- palette <$> IORef.readIORef (loaded shell)
          let seatOf' (i, seat@(name, _))
                | i == 0 = History.MkSeat History.MkYou (DeckBrowser.label (DeckBrowser.titleIn seat) name) (Just name) (Map.findWithDefault Set.empty name palette')
                | Map.member (i - 1) invited = History.MkSeat History.MkPerson (DeckBrowser.label (DeckBrowser.titleIn seat) name) Nothing Set.empty
                | Map.member (i - 1) here = History.MkSeat History.MkPerson (DeckBrowser.label (DeckBrowser.titleIn seat) name) (Just name) (Map.findWithDefault Set.empty name palette')
                | otherwise = History.MkSeat History.MkBot (DeckBrowser.label (DeckBrowser.titleIn seat) name) (Just name) (Map.findWithDefault Set.empty name palette')
          remember shell (fmap seatOf' (zip [0 :: Int ..] (NonEmpty.toList seats))) played
          lost <- IORef.readIORef down
          let go said = do
                Browser.display (page shell) (endingScreen ending)
                woke <- await shell
                case woke of
                  MkTapped (Screen.MkNav section) _ -> closeAll >> pure (sectionNext section)
                  MkTapped (Screen.MkEnding Ending.MkRematch) _
                    | Map.null invited -> play shell seats invited here Nothing
                    | otherwise -> do
                        -- A slot whose channel closed in the game is invited afresh.
                        back <- Monad.foldM (\l i -> reinvite i connectionClosed l) (Lobby.rematch seats invited here) (Set.toList lost)
                        lobby shell back (reverse said) Nothing
                  MkTapped (Screen.MkEnding Ending.MkNewGame) _ -> closeAll >> pure MkToHome
                  MkHeard heard' -> go (heard' : said)
                  _ -> go said
          go []

-- | How a game ended, under the top bar with no section current.
endingScreen :: Ending.Ending -> Html.Html Screen.Event
endingScreen = Bar.framed Screen.MkNav Nothing . fmap Screen.MkEnding . Ending.view

-- | Keeps a game this page played among these seats, once it is over with
-- a result (History.recordOf), dropping the oldest kept past the cap.
remember :: Shell -> [History.Seat] -> Ending.Played -> IO ()
remember shell seats played = do
  (at, ahead) <- Browser.clock
  Monad.forM_ (History.recordOf at ahead (build shell) seats played) $ \(game, scenario) -> do
    (kept, dropped) <- History.adding game <$> IORef.readIORef (finished shell)
    Browser.keepPlayed (History.key game) (History.encode game) scenario
    mapM_ (Browser.forgetPlayed . History.key) dropped
    IORef.writeIORef (finished shell) kept

-- | Invited slot `i`'s channel, closed at once while `down` holds the slot,
-- and held there once it closes.
tracked :: IORef.IORef (Set.Set Int) -> Int -> Link.Channel IO -> Link.Channel IO
tracked down i channel =
  channel
    { Link.receive = do
        closed <- Set.member i <$> IORef.readIORef down
        if closed
          then pure Nothing
          else do
            heard' <- Link.receive channel
            Monad.when (Maybe.isNothing heard') (IORef.modifyIORef' down (Set.insert i))
            pure heard'
    }

-- | The screen a game with a guest shows once their connection over data
-- channel `n` drops (Parun.Web.Rejoin), until a new pair of codes connects
-- the pages again (True), the screen the game showed back, or the game is
-- given up. The host's page makes a new link at once, and again whenever
-- the code pasted does not connect; the guest's makes its reply from the
-- link pasted.
rejoin :: Shell -> Int -> Rejoin.Rejoin -> IO Bool
rejoin shell n initial = do
  before <- IORef.readIORef (Browser.shown (page shell))
  Monad.when hosting (Browser.linkHost n)
  let go shown = do
        Browser.display (page shell) (fmap Screen.MkRejoin (Rejoin.view shown))
        woke <- awaitLink shell n
        case woke of
          MkHeard heard' -> case Text.unpack (Browser.kind heard') of
            kind | kind == (if hosting then "offer" else "reply") -> go shown {Rejoin.code = Just (Browser.said heard')}
            "open" -> mapM_ (Browser.display (page shell)) before >> pure True
            "closed" -> do
              Monad.when hosting (Browser.linkHost n)
              go shown {Rejoin.code = Nothing, Rejoin.typed = Text.empty, Rejoin.problem = Just (Text.pack (if hosting then "That code did not connect: send them the new link." else "That link did not connect: ask the host for a new one."))}
            _ -> go shown
          MkTapped (Screen.MkRejoin Rejoin.MkConnect) _
            | hosting -> Browser.linkAccept n (Text.strip (Rejoin.typed shown)) >> go shown {Rejoin.problem = Nothing}
            | otherwise -> Browser.linkJoin n (Rejoin.offerIn (Rejoin.typed shown)) >> go shown {Rejoin.problem = Nothing}
          MkTapped (Screen.MkRejoin Rejoin.MkGiveUp) _ -> Browser.linkClose n >> pure False
          MkTapped (Screen.MkRejoin event) value -> go (Rejoin.update event value shown)
          _ -> go shown
  go initial
  where
    hosting = Rejoin.side initial /= Rejoin.MkGuest

-- | A tap waiting, else what data channel `n` said, waiting for either; what
-- the others say stays for later.
awaitLink :: Shell -> Int -> IO Woke
awaitLink shell n = do
  waiting <- Browser.pending (page shell)
  if waiting
    then uncurry MkTapped <$> Browser.next (page shell)
    else Browser.takeLinkedWith (page shell) ((== n) . Browser.link) >>= maybe (Browser.idle >> awaitLink shell n) (pure . MkHeard)

-- | The screen another browser's link opens, joining its game over channel
-- 0 with the code the link carries: the player's deck chosen and sent, the
-- reply code made, and the match-up shown as the host tells it, until the
-- game starts or the player goes home.
joinGame :: Shell -> Text.Text -> IO Next
joinGame shell offer = do
  need shell Loading.MkDecks
  need shell Loading.MkCards
  need shell Loading.MkRules
  go Join.new
  where
    n :: Int
    n = 0
    hostChannel = Browser.channel (page shell) (build shell) n
    go shown = do
      loaded' <- IORef.readIORef (loaded shell)
      let current = case deckFiles loaded' of
            Just named | Join.reading shown -> shown {Join.reading = False, Join.decks = Seq.fromList (Map.keys named), Join.titles = titles loaded'}
            _ -> shown
      Browser.display (page shell) (framed Bar.MkPlay (fmap Screen.MkJoin (Join.view current)))
      woke <- await shell
      case woke of
        MkTapped (Screen.MkNav section) _
          | section /= Bar.MkPlay -> Browser.linkClose n >> pure (sectionNext section)
        MkHeard heard' | Browser.link heard' == n -> case Text.unpack (Browser.kind heard') of
          "reply" -> go current {Join.reply = Just (Browser.said heard')}
          "open" -> hello current >> go current {Join.connected = True, Join.problem = Nothing}
          "message" -> case Link.decode (build shell) (Browser.said heard') of
            Left problem -> Browser.linkClose n >> go (lost (capitalised problem <> Text.pack ".") current)
            Right (Link.MkMatchup places) -> go current {Join.places = places}
            Right (Link.MkStart seat seen) -> guestGame current seat seen
            Right _ -> go current
          "closed" -> go (lost hostClosed current)
          _ -> go current
        MkTapped (Screen.MkJoin event) value -> case event of
          Join.MkMakeReply -> Browser.linkJoin n offer >> go current
          Join.MkReady | Join.connected current -> Link.send hostChannel (Link.MkReady (not (Join.isReady current))) >> go current
          _ -> do
            let next = Join.update event value current
            Monad.when (Join.connected next && Join.picked next /= Join.picked current) (hello next)
            go next
        _ -> go current
    hostClosed = Text.pack "The connection to the host closed: ask them for a new link."
    lost problem current = current {Join.connected = False, Join.reply = Nothing, Join.places = [], Join.problem = Just problem}
    -- The deck chosen, sent to the host.
    hello current = do
      files <- Maybe.fromMaybe Map.empty . deckFiles <$> IORef.readIORef (loaded shell)
      Monad.forM_ (Join.picked current >>= \name -> (,) name <$> Map.lookup name files) $ \(name, path) -> do
        contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
        Link.send hostChannel (Link.MkHello name contents)
    -- The game as this seat, its log starting from what the host says it
    -- has seen, once the registry is built for the cards' text; then its
    -- ending, the channel kept open, until the host's
    -- Rematch tells the match-up again, the player's Rematch goes back to
    -- it, or the player goes home.
    guestGame current seat seen = do
      Browser.display (page shell) (message ["Loading the cards\x2026"])
      built <- untilBuilt shell (loadingScreen shell) [Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
      case built of
        Left problem -> Browser.linkClose n >> notice shell [problem]
        Right reg -> do
          chosen <- IORef.newIORef MkToHome
          played <- Guest.play (page shell) (Identity.runIdentity . Interpreter.chosenCard reg) (level shell) hostChannel (rejoin shell n Rejoin.guest) seat seen (leaveFor shell chosen)
          case played of
            Ending.MkAbandoned -> Browser.linkClose n >> IORef.readIORef chosen
            Ending.MkFinished ending -> do
              palette' <- palette <$> IORef.readIORef (loaded shell)
              let placed place
                    | Link.who place == Text.pack "You" = History.MkSeat History.MkYou (Link.deck place) (Join.picked current) (maybe Set.empty (\name -> Map.findWithDefault Set.empty name palette') (Join.picked current))
                    | Text.isPrefixOf (Text.pack "Bot") (Link.who place) = History.MkSeat History.MkBot (Link.deck place) Nothing Set.empty
                    | otherwise = History.MkSeat History.MkPerson (Link.deck place) Nothing Set.empty
              remember shell (fmap placed (Join.places current)) played
              let back = Join.rematched current
                  ended closed = do
                    Browser.display (page shell) (endingScreen ending)
                    woke <- await shell
                    case woke of
                      MkTapped (Screen.MkNav section) _ -> Browser.linkClose n >> pure (sectionNext section)
                      MkHeard heard' | Browser.link heard' == n -> case Text.unpack (Browser.kind heard') of
                        "message" | Right (Link.MkMatchup places) <- Link.decode (build shell) (Browser.said heard') -> go back {Join.places = places}
                        "closed" -> ended True
                        _ -> ended closed
                      MkTapped (Screen.MkEnding Ending.MkRematch) _
                        | closed -> go (lost hostClosed back)
                        | otherwise -> go back
                      MkTapped (Screen.MkEnding _) _ -> Browser.linkClose n >> pure MkToHome
                      _ -> ended closed
              -- A game stopped on a dropped connection left it closed.
              ended (Ending.verdict ending == Ending.MkStopped)

-- | The home screen with a game paused at this board, the replays, the
-- builder and the lobby reachable from it, until the player resumes the
-- game or gives it up for another, recorded in `chosen`; a game played on
-- from a replay is asked about on the home screen first, and one from the
-- lobby at its Start. What the game's data channels say meanwhile is kept
-- for it (Browser.aside).
leaveFor :: Shell -> IORef.IORef Next -> Maybe PlayerView.PlayerView -> IO Hotseat.Leave
leaveFor shell chosen board = Browser.aside (page shell) (go paused)
  where
    paused = Home.new {Home.paused = Just (Home.pausedOn board)}
    go shown = do
      located MkToHome
      outcome <- home shell shown
      case outcome of
        Home.MkGo choice
          | Home.starts choice -> IORef.writeIORef chosen (afterHome outcome) >> pure Hotseat.MkAbandon
          | otherwise -> visit (afterHome outcome)
        _ -> pure Hotseat.MkResume
    visit next = case next of
      MkToReplays -> located next >> replays shell >>= visit
      MkToReplay source at -> located next >> replay shell source at >>= visit
      MkToMatchup decks -> located next >> lobby shell (matchup decks) {Lobby.paused = True} [] Nothing >>= visit
      MkToBuilder deck back -> located next >> builder shell deck back >>= visit
      MkToPlayOn source at -> go paused {Home.confirming = Just (Home.MkPlayOn source at)}
      MkToDecks deck -> located next >> decksScreen shell deck >>= visit
      MkToLobby deck -> located next >> lobbyHolding shell deck >>= \shown -> lobby shell shown {Lobby.paused = True} [] Nothing >>= visit
      MkToConceded _ _ -> IORef.writeIORef chosen next >> pure Hotseat.MkAbandon
      MkToPaused -> pure Hotseat.MkResume
      _ -> go paused

-- | The replays screen, the scenarios fetched first, until a replay is
-- opened and left for a game or the home screen: the games finished here,
-- each to watch, play again or delete, deleting asked about first.
replays :: Shell -> IO Next
replays shell = do
  need shell Loading.MkScenarios
  let go shown = do
        now <- IORef.readIORef (loaded shell)
        listed <- case scenarioList now of
          Just known -> pure (Just known)
          Nothing
            | Just problem <- Loading.problemFor Loading.MkScenarios (loading now) -> pure (Just (Left problem, Map.empty, []))
            | Loading.ready Loading.MkScenarios (loading now) -> do
                known <- Replay.listing
                IORef.modifyIORef' (loaded shell) $ \l -> l {scenarioList = Just known}
                pure (Just known)
            | otherwise -> pure Nothing
        let watching source = do
              located (MkToReplay source 0)
              next <- replay shell source 0
              case next of
                MkToReplays -> located next >> go shown
                _ -> pure next
        case listed of
          Just (Left problem, _, []) | Web.Replay.shelf shown == Web.Replay.MkScenarios -> notice shell [problem]
          Just (Left problem, _, _) | Web.Replay.shelf shown == Web.Replay.MkScenarios -> notice shell [Text.pack "The scenario manifest does not decode:", problem]
          _ -> do
            Browser.display (page shell) . framed Bar.MkReplays . fmap Screen.MkReplay $ Web.Replay.picker shown (fmap (\(themes, described, names) -> (Either.fromRight Map.empty themes, described, names)) listed)
            woke <- await shell
            case woke of
              MkTapped (Screen.MkNav section) _
                | section /= Bar.MkReplays -> pure (sectionNext section)
              MkTapped (Screen.MkReplay (Web.Replay.MkOpen name)) _ -> watching (Web.Replay.MkScenario name)
              MkTapped (Screen.MkReplay (Web.Replay.MkWatch k)) _ -> watching (Web.Replay.MkPlayed k)
              MkTapped (Screen.MkReplay (Web.Replay.MkPlayAgain k)) _
                | Just decks <- List.find ((== k) . History.key) (Web.Replay.games shown) >>= traverse History.deckName . History.seats >>= NonEmpty.nonEmpty -> pure (MkToMatchup decks)
              MkTapped (Screen.MkReplay Web.Replay.MkConfirmForget) _ -> do
                case Web.Replay.forgetting shown of
                  Just (Web.Replay.MkForgetGame k) -> Browser.forgetPlayed k >> IORef.modifyIORef' (finished shell) (filter ((/= k) . History.key))
                  Just Web.Replay.MkForgetAll -> Browser.clearPlayed >> IORef.writeIORef (finished shell) []
                  Nothing -> pure ()
                go (Web.Replay.forgot shown)
              MkTapped (Screen.MkReplay event) value -> go (Web.Replay.listed event value shown)
              _ -> go shown
  go . Web.Replay.listing =<< IORef.readIORef (finished shell)

-- | The replay, once it and the cards have loaded, from this position until
-- the page leaves it.
replay :: Shell -> Web.Replay.Source -> Natural -> IO Next
replay shell source at = opened shell source $ \reg title seen scenario -> do
  let moved = Browser.locate Browser.MkReplace . Route.render . routeOf . MkToReplay source
  exit <- Replay.viewer (page shell) moved reg title seen scenario at
  pure $ case exit of
    Replay.MkToList -> MkToReplays
    Replay.MkPlayOn from -> MkToPlayOn source from

-- | A game played on from a position of the replay, then how it ended,
-- with the way back to the position and to play on again; given up from
-- the home screen for another game, that one.
playOnFrom :: Shell -> Web.Replay.Source -> Natural -> IO Next
playOnFrom shell source at = opened shell source $ \reg _ played scenario -> do
  chosen <- IORef.newIORef MkToHome
  ended <- Replay.playOn (page shell) reg (level shell) scenario at (leaveFor shell chosen)
  case ended of
    Nothing -> IORef.readIORef chosen
    Just said' -> do
      let said = foldMap (Maybe.maybeToList . History.drift (build shell)) played <> said'
      let go = do
            Browser.display (page shell) . fmap Screen.MkReplay $ Web.Replay.ended said
            woke <- await shell
            case woke of
              MkTapped (Screen.MkReplay Web.Replay.MkResumeReplay) _ -> pure (MkToReplay source at)
              MkTapped (Screen.MkReplay Web.Replay.MkBranch) _ -> pure (MkToPlayOn source at)
              _ -> go
      go

-- | The registry and the replay, its title, the game finished here it is,
-- if it is one, and its scenario, handed on once loaded, a message shown
-- meanwhile; else why either did not load. A scenario is titled from its
-- description and described by its rules citation (Web.Replay.titled); a
-- game finished here waits on no scenarios, and is described by a note
-- where another build played it (History.drift); how it ended waits for
-- the replay's end (Replay.viewer).
opened :: Shell -> Web.Replay.Source -> (Registry.Registry Identity.Identity -> Text.Text -> Maybe History.Game -> Scenario.Type.Scenario -> IO Next) -> IO Next
opened shell source continue = do
  Browser.display (page shell) (message ["Loading the cards\x2026"])
  let needs = [Loading.MkCards, Loading.MkRules] <> [Loading.MkScenarios | Web.Replay.MkScenario _ <- [source]]
  built <- untilBuilt shell (loadingScreen shell) needs $ do
    now <- IORef.readIORef (loaded shell)
    pure (if all (`Loading.ready` loading now) needs then registry now else Nothing)
  case built of
    Left problem -> notice shell [problem]
    Right reg -> case source of
      Web.Replay.MkScenario name -> do
        decoded <- Replay.decode name
        either (\problem -> notice shell [Text.pack ("The scenario " <> Text.unpack name <> " does not decode:"), Text.pack problem]) (\scenario -> let (title, rules) = Web.Replay.titled name (Scenario.Type.description scenario) in continue reg title Nothing scenario {Scenario.Type.description = rules}) decoded
      Web.Replay.MkPlayed k -> do
        game <- List.find ((== k) . History.key) <$> IORef.readIORef (finished shell)
        file <- Browser.playedScenario k
        case (game, History.scenarioOf file) of
          (Nothing, _) -> notice shell [Text.pack "That game is no longer kept here."]
          (_, Left problem) -> notice shell [Text.pack "The game kept does not decode:", problem]
          (Just g, Right scenario) -> continue reg (History.matchup g) (Just g) scenario {Scenario.Type.description = Maybe.fromMaybe Text.empty (History.drift (build shell) g)}

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

-- | A tap waiting, else what a data channel said, else a chunk arrived,
-- else one batch of loading work, else whatever comes next: a waiting tap
-- is never kept behind work.
await :: Shell -> IO Woke
await shell = do
  waiting <- Browser.pending (page shell)
  if waiting
    then uncurry MkTapped <$> Browser.next (page shell)
    else do
      heard' <- Browser.takeLinked (page shell)
      case heard' of
        Just linked -> pure (MkHeard linked)
        Nothing -> do
          progressed <- awaitWork shell
          if progressed
            then pure MkChanged
            else do
              input <- Browser.nextInput (page shell)
              case input of
                Browser.MkTap event value -> pure (MkTapped event value)
                Browser.MkArrival a -> arrived shell [a] >> pure MkChanged
                Browser.MkLink linked -> pure (MkHeard linked)

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
            size <- Exception.evaluate (Lobby.mainCount contents)
            shape <- Exception.evaluate (DeckBrowser.curveOf [lineOf root byFile n card | (n, card) <- Lobby.mainEntries contents])
            _ <- Exception.evaluate (sum shape)
            Exception.evaluate . (,) name . (,,) size shape $! Lobby.deckColours colours contents
          IORef.modifyIORef' (loaded shell) $ \l -> l {palette = Map.union (Map.fromList [(name, colours') | (name, (_, _, colours')) <- read']) (palette l), deckSizes = Map.union (Map.fromList [(name, size) | (name, (size, _, _)) <- read']) (deckSizes l), deckCurves = Map.union (Map.fromList [(name, shape) | (name, (_, shape, _)) <- read']) (deckCurves l), unread = rest}
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
            (,) name . fst <$> deckProblems shell reg contents
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
      texts <- traverse (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) files
      let lists = Map.fromList [(code, path) | path <- paths, Just code <- [Text.stripPrefix (Text.pack "sets/") (Lobby.deckName deckRoot path) >>= Text.stripSuffix (Text.pack "/all")]]
      sets <- DeckBrowser.releasesOf . Encoding.decodeUtf8Lenient <$> (Decks.setsPath >>= ByteString.readFile)
      Monad.forM_ (Either.lefts [sets]) $ \problem -> Browser.logLine (Text.pack "The sets did not decode: " <> problem)
      IORef.modifyIORef' (loaded shell) $ \l -> l {deckFiles = Just files, formats = Map.mapMaybe DeckList.formatOf texts, titles = Map.mapMaybe DeckList.nameOf texts, unread = Map.toList files, unchecked = filter (DeckBrowser.isSaved . fst) (Map.toList files), setLists = lists, releases = Either.fromRight Map.empty sets}
    readCards = do
      root <- Registry.defaultRoot
      bundles <- Registry.bundlePaths >>= traverse ByteString.readFile
      let byFile = LazyMap.fromList (Registry.loadBundles root bundles)
      _ <- Exception.evaluate (LazyMap.size byFile)
      IORef.modifyIORef' (loaded shell) $ \l -> l {cards = Just byFile}

message :: [String] -> Html.Html msg
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Element.string t] | t <- said]
