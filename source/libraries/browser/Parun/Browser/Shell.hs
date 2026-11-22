-- | The web client as one loop (#9075): the page, the screen it shows, and
-- the data loaded so far, built chunk by chunk (Parun.Web.Loading) one
-- batch at a time while no tap waits.
module Parun.Browser.Shell where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
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
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Key as Key
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.CardIndex as CardIndex
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Join as Join
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Loading as Loading
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Rejoin as Rejoin
import qualified Parun.Web.Replay as Web.Replay
import qualified Parun.Web.Screen as Screen
import qualified System.Environment as Environment
import qualified Text.Read as Read

data Shell = MkShell
  { page :: Browser.Page Screen.Event,
    loaded :: IORef.IORef Loaded,
    -- | How much the page answers for the player (Web.Question.Level), as
    -- the last game left it.
    level :: IORef.IORef Web.Question.Level,
    -- | The build the page runs, which a page it links to must share
    -- (Parun.Web.Link).
    build :: Text.Text
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
    setLists :: Map.Map Text.Text FilePath,
    -- | How many cards each deck's main deck holds, by name, as read so far.
    deckSizes :: Map.Map Text.Text Natural
  }

-- | Where a screen leads when it ends.
data Next
  = MkToHome
  | MkToLobby
  | -- | The lobby, a bot's seat given to a player invited by link.
    MkToInvite
  | -- | The builder, on this deck if any, its Back leading here.
    MkToBuilder (Maybe Text.Text) Next
  | MkToReplays
  | -- | This scenario's replay, at this position.
    MkToReplay Text.Text Natural
  | -- | A game played on from this position of this scenario.
    MkToPlayOn Text.Text Natural
  | -- | Another browser's game, joined with this code from its link.
    MkToJoin Text.Text
  | -- | The game with guests the page kept, picked up where it stopped.
    MkToRestore Link.Saved

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
  shell <- MkShell <$> Browser.mount <*> IORef.newIORef (MkLoaded Loading.new Nothing Map.empty Nothing Map.empty [] Nothing Nothing Nothing False Nothing Map.empty [] Nothing Map.empty Map.empty) <*> IORef.newIORef chosen <*> pure built
  Exception.catch (opening built >>= follow shell) $ \problem ->
    Browser.display (page shell) $ message ["The page stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

-- | Where the page's link leads, read once (engine.js passes its query on):
-- another browser's game to join (parun_offer, from the link's #offer=); a
-- scenario's replay (parun_scenario, parun_at), played on from there
-- (parun_branch), or the list of them for the empty name; else the game
-- with guests the page kept, played on this build; else the home screen.
-- A kept game that does not read is forgotten.
opening :: Text.Text -> IO Next
opening build' = do
  offered <- Environment.lookupEnv "parun_offer"
  named <- Environment.lookupEnv "parun_scenario"
  at <- Maybe.fromMaybe 0 . (>>= Read.readMaybe) <$> Environment.lookupEnv "parun_at"
  branching <- Maybe.isJust <$> Environment.lookupEnv "parun_branch"
  kept <- Browser.keptGame
  case (offered, named) of
    (Just code, _) | not (null code) -> pure (MkToJoin (Text.pack code))
    (_, Just "") -> pure MkToReplays
    (_, Just name) -> pure ((if branching then MkToPlayOn else MkToReplay) (Text.pack name) at)
    _
      | Text.null kept -> pure MkToHome
      | otherwise -> either (\problem -> Browser.logLine (Text.pack "The game kept does not read: " <> problem) >> Browser.forgetGame >> pure MkToHome) (pure . MkToRestore) (Link.unsaved build' kept)

-- | Goes where a screen led, and on from there.
follow :: Shell -> Next -> IO ()
follow shell next = case next of
  MkToHome -> home shell Home.new >>= follow shell . afterHome
  MkToLobby -> lobby shell Lobby.new [] >>= follow shell
  MkToInvite -> Browser.linkHost 0 >> lobby shell (Lobby.update (Lobby.MkInviteSlot 0) Text.empty Lobby.new) [] >>= follow shell
  MkToBuilder deck back -> builder shell deck back >>= follow shell
  MkToReplays -> replays shell >>= follow shell
  MkToReplay name at -> replay shell name at >>= follow shell
  MkToPlayOn name at -> playOnFrom shell name at >>= follow shell
  MkToJoin code -> joinGame shell code >>= follow shell
  MkToRestore kept -> play shell (Link.seats kept) (Set.fromList (Link.invited kept)) (Just kept) >>= follow shell

-- | Where the home screen leads.
afterHome :: Home.Outcome -> Next
afterHome outcome = case outcome of
  Home.MkGo Home.MkPlayBots -> MkToLobby
  Home.MkGo Home.MkPlayFriend -> MkToInvite
  Home.MkGo Home.MkBuildDeck -> MkToBuilder Nothing MkToHome
  Home.MkGo (Home.MkEditDeck name) -> MkToBuilder (Just name) MkToHome
  Home.MkGo Home.MkReplays -> MkToReplays
  Home.MkGo (Home.MkPlayOn name at) -> MkToPlayOn name at
  _ -> MkToHome

-- | Where a section of the top bar leads.
sectionNext :: Bar.Section -> Next
sectionNext section = case section of
  Bar.MkHome -> MkToHome
  Bar.MkPlay -> MkToLobby
  Bar.MkDecks -> MkToBuilder Nothing MkToHome
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
      let current =
            shown
              { Home.problem = Loading.problem (loading now),
                Home.saved = [Home.MkDeck name (Map.findWithDefault Set.empty name (palette now)) (Map.lookup name (deckSizes now)) | name <- foldMap Map.keys (deckFiles now), Lobby.isSaved name]
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

-- | The lobby, from this one once the invited slots' channels said these,
-- until it goes home or starts a game, filling in as decks and cards
-- arrive, and as the players invited from other browsers connect and
-- choose (Parun.Web.Link), each told the match-up at every change; where
-- the game, played from here, then leads.
lobby :: Shell -> Lobby.Lobby -> [Browser.Linked] -> IO Next
lobby shell initial said = do
  need shell Loading.MkDecks
  need shell Loading.MkCards
  heard' <- Monad.foldM heardBy initial said
  go' heard' Map.empty
  where
    go' shown told = do
      current <- refreshed shell shown
      told' <- tellPlaces current told
      if Lobby.starts current
        then startWith current told'
        else do
          Browser.display (page shell) (framed Bar.MkPlay (fmap Screen.MkLobby (Lobby.view current)))
          woke <- await shell
          -- Every invite the lobby drops has its channel closed.
          let go next = do
                mapM_ Browser.linkClose (Map.keys (Map.difference (Lobby.invites current) (Lobby.invites next)))
                go' next (Map.restrictKeys told' (Map.keysSet (Lobby.invites next)))
              leaving next = mapM_ Browser.linkClose (Map.keys (Lobby.invites current)) >> pure next
          case woke of
            MkHeard heard' -> heardBy current heard' >>= go
            MkTapped (Screen.MkNav section) _
              | section /= Bar.MkPlay -> leaving (sectionNext section)
            MkTapped (Screen.MkLobby event) value -> case event of
              Lobby.MkOpenBuilder -> leaving (MkToBuilder Nothing MkToLobby)
              Lobby.MkEdit name -> leaving (MkToBuilder (Just name) MkToLobby)
              Lobby.MkStart
                | Map.null (Lobby.invites current),
                  Right mine <- Lobby.startable current -> do
                    others <- traverse (maybe (randomOf (Lobby.opponents current)) (pure . Just)) (Lobby.theirs current)
                    files <- Maybe.fromMaybe Map.empty <$> decksRead shell
                    -- A bot with no deck to play keeps the lobby up.
                    case sequence others >>= traverse (\name -> (,) name <$> Map.lookup name files) . (mine NonEmpty.:|) of
                      Just seats -> listsOf seats >>= \lists -> play shell lists Set.empty Nothing
                      Nothing -> go current
              Lobby.MkInviteSlot i -> Browser.linkHost i >> go (Lobby.update event value current)
              Lobby.MkConnect i | Just invite <- Map.lookup i (Lobby.invites current) -> do
                Browser.linkAccept i (Text.strip (Lobby.replyCode invite))
                go current
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
    -- What an invited slot's channel said: its link, its opening, a
    -- message, or its closing, which makes a new link to send.
    heardBy current heard' =
      let i = Browser.link heard'
       in case Text.unpack (Browser.kind heard') of
            _ | Map.notMember i (Lobby.invites current) -> pure current
            "offer" -> pure (Lobby.linked i (\invite -> invite {Lobby.link = Just (Browser.said heard')}) current)
            "open" -> pure (Lobby.linked i (\invite -> invite {Lobby.connected = True, Lobby.inviteProblem = Nothing}) current)
            "message" -> case Link.decode (build shell) (Browser.said heard') of
              Right told -> pure (Lobby.heard i told current)
              Left problem -> reinvite i (capitalised problem <> Text.pack ".") current
            "closed" -> reinvite i connectionClosed current
            _ -> pure current
    -- Each connected player told the match-up where it changed since.
    tellPlaces current told =
      fmap Map.fromList . Monad.forM [(i, Lobby.placesFor i current) | (i, invite) <- Map.toList (Lobby.invites current), Lobby.connected invite] $ \(i, places) -> do
        Monad.when (Map.lookup i told /= Just places) $ Link.send (Browser.channel (page shell) (build shell) i) (Link.MkMatchup places)
        pure (i, places)
    -- Every player is ready: the game starts, each player invited told
    -- their seat, each bot without a deck given one at random.
    startWith current told = do
      others <- traverse (maybe (randomOf (Lobby.opponents current)) (pure . Just)) (Lobby.theirs current)
      files <- Maybe.fromMaybe Map.empty <$> decksRead shell
      let invited = Lobby.invites current
          slot (i, name) = case Map.lookup i invited >>= Lobby.guestDeck of
            Just (theirs, contents) -> pure (Just (theirs, contents))
            Nothing -> traverse (\(held, path) -> (,) held . Encoding.decodeUtf8Lenient <$> ByteString.readFile path) (name >>= \held -> (,) held <$> Map.lookup held files)
      mineFile <- traverse (\name -> traverse (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) (Map.lookup name files)) (Lobby.mine current)
      seated <- traverse slot (zip [0 ..] others)
      case (,) <$> Lobby.mine current <*> Monad.join mineFile of
        Just mine | Just rest <- sequence seated -> do
          Monad.forM_ (Map.keys invited) $ \i -> Link.send (Browser.channel (page shell) (build shell) i) (Link.MkStart (seatOf i))
          play shell (mine NonEmpty.:| rest) (Map.keysSet invited) Nothing
        -- A bot with no deck to play keeps the lobby up.
        _ -> go' current {Lobby.ready = False} told

-- | The lobby with invited slot `i` made afresh, a new link made for it,
-- saying why.
reinvite :: Int -> Text.Text -> Lobby.Lobby -> IO Lobby.Lobby
reinvite i problem current = do
  Browser.linkHost i
  pure (Lobby.linked i (const Lobby.newInvite {Lobby.inviteProblem = Just problem}) current)

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

-- | The lobby once a pasted deck list is imported under the name typed:
-- checked (Command.check) once the registry is built, "Checking" shown
-- meanwhile, then saved and made the player's; else the panel says why not.
importDeck :: Shell -> Lobby.Lobby -> Lobby.Import -> IO Lobby.Lobby
importDeck shell current panel = case Lobby.nameProblems (Lobby.decks current) (Lobby.importName panel) of
  problems@(_ : _) -> pure (refuse problems)
  [] -> do
    Browser.display (page shell) . framed Bar.MkPlay . fmap Screen.MkLobby $ Lobby.view current {Lobby.importing = Just panel {Lobby.checking = True, Lobby.importProblems = []}}
    built <- untilBuilt shell quiet [Loading.MkDecks, Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
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
        Lobby.sizes = deckSizes now,
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
         in -- CR 709.4b: a split card's cost and mana value are its halves'.
            Lobby.MkLine n name (Face.manaCost (EngineCard.combined c)) (Quantity.manaValueOf (EngineCard.combined c)) kind
  _ -> Lobby.MkLine n name Nothing 0 Lobby.MkSpell

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
  initial <- case deck >>= \name -> (,) name <$> Map.lookup name files of
    Just (name, path) -> do
      contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
      pure (Builder.open (if Lobby.isSaved name then Just name else Nothing) (snd (Lobby.describe name)) contents (Builder.new []))
    Nothing -> pure (Builder.new []) {Builder.notices = [Text.pack "That deck could not be opened, so this is a new one." | Maybe.isJust deck]}
  go initial Nothing back
  where
    -- The builder, the list it was last checked at, and where leaving it
    -- leads.
    go shown lastChecked leaving = do
      now <- IORef.readIORef (loaded shell)
      named <- if null (Builder.formats shown) then Legality.shippedFormats else pure (Builder.formats shown)
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
            | otherwise = pure next
      case woke of
        MkTapped (Screen.MkNav section) _
          | section /= Bar.MkDecks -> leave (sectionNext section)
        MkTapped (Screen.MkBuilder event) value -> case event of
          Builder.MkBack -> leave back
          Builder.MkLeave -> pure leaving
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

-- | Why a deck list breaks the format its header names, as lines, and the
-- cards those lines are about (Legality.namedCard). No card pool is given:
-- a pooled list writes its pool's other cards as its sideboard (CR 100.4b),
-- so the pool is the list's own, and the builder caps each card at it.
-- The legality table is loaded once, at the first check, and kept.
deckProblems :: Shell -> Registry.Registry Identity.Identity -> Text.Text -> IO ([Text.Text], Set.Set CardName.CardName)
deckProblems shell reg contents = case Command.loadText reg "deck" contents of
  Left problems -> pure (fmap (\p -> Maybe.fromMaybe p (Text.stripPrefix (Text.pack "deck: ") p)) problems, Set.empty)
  Right listed -> do
    kept <- legalityTable <$> IORef.readIORef (loaded shell)
    table <- case kept of
      Just t -> pure t
      Nothing -> do
        t <- Legality.tablePath >>= Legality.loadTable
        IORef.modifyIORef' (loaded shell) $ \l -> l {legalityTable = Just t}
        pure t
    let lookUp = either (const . pure . Left) Legality.namedIn table
    (_, lines', violations) <- Legality.assess lookUp Nothing Nothing listed
    pure (lines' <> fmap Legality.explain violations, Set.fromList (Maybe.mapMaybe Legality.namedCard violations))

-- | One of these decks at random.
randomOf :: Seq.Seq Text.Text -> IO (Maybe Text.Text)
randomOf among = do
  key <- Key.fresh
  let roll = foldr (\w acc -> acc * 4294967296 + toInteger w) 0 (Key.unwrap key)
  pure (Integer.toInt (mod roll (toInteger (max 1 (Seq.length among)))) >>= (`Seq.lookup` among))

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
-- registry is built, shown while it builds, each bot's slot in `invited`
-- played from another browser over its channel (Parun.Web.Link), or that
-- game picked up where `resuming` stopped; its ending, and the games its
-- Rematch starts; then where it leads. A game with guests is kept until it
-- is over (Link.saved), and a guest whose connection drops, or every guest
-- of a game picked up, is asked to rejoin (rejoin) when next asked. Home
-- from the game shows the home screen with the game kept; a new game chosen
-- there gives it up. Every player invited is told how it ended; a Rematch
-- with them goes back to the lobby on the match-up played (Lobby.rematch),
-- their channels kept open and what they said meanwhile heard there, and a
-- new game closes them.
play :: Shell -> NonEmpty.NonEmpty (Text.Text, Text.Text) -> Set.Set Int -> Maybe Link.Saved -> IO Next
play shell seats invited resuming = do
  Browser.display (page shell) (if Maybe.isJust resuming then message ["Picking the game up where it stopped\x2026"] else Hotseat.starting (fmap fst seats))
  built <- untilBuilt shell (loadingScreen shell) [Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
  -- The invited slots whose connection is down: every one of a game picked up.
  down <- IORef.newIORef (if Maybe.isJust resuming then invited else Set.empty)
  let channelOf i = tracked down i (Browser.channel (page shell) (build shell) i)
      tellAll verdictFor said scenario = Monad.forM_ invited $ \i -> Link.send (channelOf i) (Link.MkEnd (verdictFor (seatOf i)) said scenario)
      stopped said = tellAll (const Ending.MkStopped) said Text.empty >> mapM_ Browser.linkClose invited
      keeping = not (Set.null invited)
  case built of
    Left problem -> stopped [problem] >> notice shell [problem]
    Right reg -> do
      chosen <- IORef.newIORef MkToHome
      seed <- maybe Key.fresh (pure . Link.seed) resuming
      Monad.when (keeping && Maybe.isNothing resuming) . Browser.keepGame $ Link.saved (build shell) (Link.MkSaved seed seats (Set.toList invited) [])
      let rejoinSlot i = do
            back <- rejoin shell i (Rejoin.host (Text.pack ("Player " <> show (i + 2))))
            Monad.when back (IORef.modifyIORef' down (Set.delete i))
            pure back
          remotes = Map.fromList [(seatOf i, Link.remote (rejoinSlot i) (channelOf i)) | i <- Set.toList invited]
          kept = if keeping then Browser.keepReply . Link.savedReply else const (pure ())
      ended <- Hotseat.game (page shell) reg (level shell) (fmap (Bifunctor.first Text.unpack) seats) remotes seed (foldMap Link.replies resuming) kept tellAll (leaveFor shell chosen)
      Monad.when keeping Browser.forgetGame
      case ended of
        Left problems -> stopped problems >> notice shell problems
        Right Hotseat.MkAbandoned -> stopped [Text.pack "The host gave the game up."] >> IORef.readIORef chosen
        Right (Hotseat.MkFinished ending) -> do
          lost <- IORef.readIORef down
          let go said = do
                Browser.display (page shell) (fmap Screen.MkEnding (Ending.view ending))
                woke <- await shell
                case woke of
                  MkTapped (Screen.MkEnding Ending.MkRematch) _
                    | Set.null invited -> play shell seats invited Nothing
                    | otherwise -> do
                        -- A slot whose channel closed in the game is invited afresh.
                        back <- Monad.foldM (\l i -> reinvite i connectionClosed l) (Lobby.rematch seats invited) (Set.toList lost)
                        lobby shell back (reverse said)
                  MkTapped (Screen.MkEnding Ending.MkNewGame) _ -> mapM_ Browser.linkClose invited >> pure MkToHome
                  MkHeard heard' -> go (heard' : said)
                  _ -> go said
          go []

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
      files <- deckFiles <$> IORef.readIORef (loaded shell)
      let current = case files of
            Just named | Join.reading shown -> shown {Join.reading = False, Join.decks = Seq.fromList (Map.keys named)}
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
            Right (Link.MkStart seat) -> guestGame current seat
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
    -- The game as this seat, once the registry is built for the cards'
    -- text; then its ending, the channel kept open, until the host's
    -- Rematch tells the match-up again, the player's Rematch goes back to
    -- it, or the player goes home.
    guestGame current seat = do
      built <- untilBuilt shell (loadingScreen shell) [Loading.MkCards, Loading.MkRules] (registry <$> IORef.readIORef (loaded shell))
      case built of
        Left problem -> Browser.linkClose n >> notice shell [problem]
        Right reg -> do
          chosen <- IORef.newIORef MkToHome
          played <- Guest.play (page shell) (Identity.runIdentity . Interpreter.chosenCard reg) (level shell) hostChannel (rejoin shell n Rejoin.guest) seat (leaveFor shell chosen)
          case played of
            Hotseat.MkAbandoned -> Browser.linkClose n >> IORef.readIORef chosen
            Hotseat.MkFinished ending -> do
              let back = Join.rematched current
                  ended closed = do
                    Browser.display (page shell) (fmap Screen.MkEnding (Ending.view ending))
                    woke <- await shell
                    case woke of
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

-- | The home screen with a game paused at this board, the replays and the
-- builder reachable from it, until the player resumes the game or gives it
-- up for another, recorded in `chosen`; a new game chosen elsewhere is
-- asked about on the home screen first.
leaveFor :: Shell -> IORef.IORef Next -> Maybe PlayerView.PlayerView -> IO Hotseat.Leave
leaveFor shell chosen board = go paused
  where
    paused = Home.new {Home.paused = Just (Home.pausedOn board)}
    go shown = do
      outcome <- home shell shown
      case outcome of
        Home.MkGo choice
          | Home.starts choice -> IORef.writeIORef chosen (afterHome outcome) >> pure Hotseat.MkAbandon
          | otherwise -> visit (afterHome outcome)
        _ -> pure Hotseat.MkResume
    visit next = case next of
      MkToReplays -> replays shell >>= visit
      MkToBuilder deck back -> builder shell deck back >>= visit
      MkToPlayOn name at -> go paused {Home.confirming = Just (Home.MkPlayOn name at)}
      MkToLobby -> go paused {Home.confirming = Just Home.MkPlayBots}
      MkToInvite -> go paused {Home.confirming = Just Home.MkPlayFriend}
      _ -> go paused

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
            Browser.display (page shell) . framed Bar.MkReplays . fmap Screen.MkReplay $ Web.Replay.picker typed (fmap (Bifunctor.first (Either.fromRight Map.empty)) listed)
            woke <- await shell
            case woke of
              MkTapped (Screen.MkNav section) _
                | section /= Bar.MkReplays -> pure (sectionNext section)
              MkTapped (Screen.MkReplay Web.Replay.MkSearch) value -> go value
              MkTapped (Screen.MkReplay (Web.Replay.MkOpen name)) _ -> do
                next <- replay shell name 0
                case next of
                  MkToReplays -> go typed
                  _ -> pure next
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
  built <- untilBuilt shell (loadingScreen shell) [Loading.MkCards, Loading.MkRules, Loading.MkScenarios] $ do
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
            Exception.evaluate . (,) name . (,) size $! Lobby.deckColours colours contents
          IORef.modifyIORef' (loaded shell) $ \l -> l {palette = Map.union (Map.fromList [(name, colours') | (name, (_, colours')) <- read']) (palette l), deckSizes = Map.union (Map.fromList [(name, size) | (name, (size, _)) <- read']) (deckSizes l), unread = rest}
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
