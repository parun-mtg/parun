-- | A game against the random bot in the browser: the lobby picks each deck
-- from the data the page mounted, then `parun play --json`'s game runs with
-- every request line answered on the page (Parun.Web.Question), and the
-- page ends on how it ended (Parun.Web.Ending).
module Parun.Browser.Hotseat where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
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
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Engine.Quantity as Quantity
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Key as Key
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Question as Web.Question
import qualified System.Environment as Environment

-- | The game on a page mounted first, so any exception that escapes it is
-- shown there rather than ending the module unseen.
start :: IO ()
start = do
  page <- Browser.mount
  Exception.catch (play page) $ \problem ->
    Browser.display page . message $ fmap Text.pack ["The game stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

play :: Browser.Page (Either Lobby.Event Web.Question.Event) -> IO ()
play page = do
  root <- Registry.defaultRoot
  loaded <- fmap (Registry.loadBundle root) $ Registry.bundlePath >>= ByteString.readFile
  deckRoot <- Decks.defaultRoot
  paths <- Decks.listRoot deckRoot
  -- A set's whole card list is no deck to play.
  let byName = Map.fromList [(name, path) | path <- paths, let name = Lobby.deckName deckRoot path, not (Text.isSuffixOf (Text.pack "/all") name)]
      names = Seq.fromList (Map.keys byName)
      random = do
        key <- Key.fresh
        let roll = foldr (\w acc -> acc * 4294967296 + toInteger w) 0 (Key.unwrap key)
        pure (Integer.toInt (mod roll (toInteger (max 1 (Seq.length names)))) >>= (\i -> Seq.lookup i names))
      -- The lobby, with the decks whose colours are still to read: one
      -- batch at a time while no tap waits, the page given a turn between.
      choose lobby work = do
        Browser.display page . fmap Left $ Lobby.view lobby
        waiting <- Browser.pending page
        case work of
          _ : _ | not waiting -> do
            let (batch, rest) = splitAt 8 work
            read' <- Monad.forM batch $ \(name, path) -> do
              contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
              Exception.evaluate . (,) name $! Lobby.deckColours colours contents
            Browser.pause
            choose lobby {Lobby.palette = Map.union (Lobby.palette lobby) (Map.fromList read')} rest
          _ -> do
            (event, value) <- Browser.next page
            case event of
              Left Lobby.MkStart | Just one <- Lobby.mine lobby >>= (`Map.lookup` byName) -> do
                two <- maybe (fmap (Maybe.fromMaybe Text.empty) random) pure (Lobby.theirs lobby)
                maybe (choose lobby work) (\path -> pure (one, path)) (Map.lookup two byName)
              Left Lobby.MkRandom -> random >>= maybe (choose lobby work) (\name -> choose (Lobby.update (Lobby.MkPick name) Text.empty lobby) work)
              Left e -> previewing (Lobby.update e value lobby) >>= \next -> choose next work
              Right _ -> choose lobby work
      -- A deck's colours come from the card files its lines name, each
      -- parsed only then.
      -- Lazy, so a file is parsed only when looked up.
      byFile = LazyMap.fromList loaded
      colours name = case LazyMap.lookup (Registry.cardPath root (Registry.slugFor name)) byFile of
        Just (Right card) -> Lobby.cardColours card
        _ -> Set.empty
      prepared = Map.toList byName
      -- The chosen decks' lists, read once each.
      previewing lobby = do
        let wanted = [name | name <- Maybe.catMaybes [Lobby.mine lobby, Lobby.theirs lobby], Map.notMember name (Lobby.previews lobby)]
        read' <- Monad.forM wanted $ \name -> do
          contents <- maybe (pure Text.empty) (fmap Encoding.decodeUtf8Lenient . ByteString.readFile) (Map.lookup name byName)
          pure (name, [line n card | (n, card) <- Lobby.entries contents])
        pure lobby {Lobby.previews = Map.union (Lobby.previews lobby) (Map.fromList read')}
      line n name = case LazyMap.lookup (Registry.cardPath root (Registry.slugFor name)) byFile of
        Just (Right c)
          | face <- NonEmpty.head (Card.faces c) ->
              let types = TypeLine.types (Face.typeLine face)
                  kind
                    | Set.member CardType.Land types = Lobby.MkLand
                    | Set.member CardType.Creature types = Lobby.MkCreature
                    | otherwise = Lobby.MkSpell
               in Lobby.MkLine n name (Face.manaCost face) (Quantity.manaValueOf face) kind
        _ -> Lobby.MkLine n name Nothing 0 Lobby.MkSpell
      say = Browser.display page . fmap Right . message
  initialLevel <- maybe Web.Question.MkStandard (Maybe.fromMaybe Web.Question.MkStandard . Web.Question.levelNamed) <$> Environment.lookupEnv "parun_level"
  -- A rematch names both decks (Ending.rematch) and skips the lobby.
  asked <- (,) <$> Environment.lookupEnv "parun_mine" <*> Environment.lookupEnv "parun_theirs"
  (one, two) <- case asked of
    (Just mine, Just theirs) | Just a <- Map.lookup (Text.pack mine) byName, Just b <- Map.lookup (Text.pack theirs) byName -> pure (a, b)
    _ -> choose (Lobby.new (Natural.length loaded) names) prepared
  -- The printing table is the slow part of the registry, so the lobby shows first.
  Browser.display page . fmap Right $ starting (Lobby.deckName deckRoot one) (Lobby.deckName deckRoot two)
  registry <- Registry.defaultRegistry loaded :: IO (Registry.Registry Identity.Identity)
  -- The bot needs no screen, so the player is never handed it.
  seatRef <- IORef.newIORef =<< newSeat initialLevel
  listed <- (,) <$> Command.load registry one <*> Command.load registry two
  case listed of
    (Right first, Right second) -> do
      settled <- Command.settle Nothing [(one, first), (two, second)]
      case settled of
        Right format -> do
          -- A shared "same shuffle" link names its seed (Ending.rematch).
          given <- (>>= Key.fromHex . Text.pack) <$> Environment.lookupEnv "parun_seed"
          seed <- maybe Key.fresh pure given
          (outcome, recorded) <- Command.game (exchange (Identity.runIdentity . Interpreter.chosenCard registry) page seatRef) options registry format (DeckList.deck first, DeckList.deck second) seed
          -- What the player saw after their last decision: the killing blow, say.
          Monad.forM_ outcome $ IORef.modifyIORef' seatRef . witness . PlayerView.Engine.viewFor player . snd
          finished <- IORef.readIORef seatRef
          let seen = loggedFor player finished
              trail = lives finished
          Browser.display page . Ending.view $ Ending.MkEnding (verdictOf outcome) (either (\reason -> [Text.pack "The game stopped: " <> reason <> Text.pack "."]) (\(result, gs) -> Render.result result : Render.departures gs) outcome) (Command.rendered recorded) seen player (Lobby.deckName deckRoot one, Lobby.deckName deckRoot two) (Foldable.toList trail) (Key.toHex seed)
        Left problems -> say problems
    (first, second) -> say $ Either.fromLeft [] first <> Either.fromLeft [] second

-- | How the game ended for the seat the page plays.
verdictOf :: Either a (Result.Result, b) -> Ending.Verdict
verdictOf outcome = case outcome of
  Left _ -> Ending.MkStopped
  Right (Result.Won pid, _) -> if pid == player then Ending.MkWon else Ending.MkLost
  Right (Result.TeamWon _, _) -> Ending.MkStopped
  Right (Result.Drawn, _) -> Ending.MkDrawn

-- | The seat the page plays.
player :: PlayerId.PlayerId
player = PlayerId.MkPlayerId 0

-- | The player is seat 1, over the line protocol; the random bot is seat 2.
options :: PlayOptions.PlayOptions
options = PlayOptions.none {PlayOptions.json = True, PlayOptions.bots = Set.singleton 2}

-- | One request answered on the page, as values (Protocol.Exchange): a reply the seat is never asked
-- for, or auto's next source once the seat chose auto for this payment, at
-- once; else after a hand-off when the screen last faced another seat, the
-- question's form until it answers, a card tapped showing its text from the
-- card data through `lookUp`.
exchange :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page (Either Lobby.Event Web.Question.Event) -> IORef.IORef Seat -> Protocol.Exchange IO
exchange lookUp page seatRef request = do
  arrived <- Clock.getMonotonicTime
  fmap (Just . Right) $ do
    Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
    state <- IORef.readIORef seatRef
    let auto = if paying state || level state == Web.Question.MkAutoPass then Web.Question.paying request else Nothing
        kind = Question.kind $ Request.question request
        traced how = Browser.trace page $ kind <> Text.pack (": engine " <> Browser.milliseconds (arrived - answeredAt state) <> ", " <> how)
        turn = fmap PlayerView.turn (Request.view request)
        -- The seat's own stop: it asked to pass until the turn ends, which
        -- holds while the stack is empty, and declares no attackers.
        skipped = do
          Monad.guard (Maybe.isJust (skipping state) && skipping state == turn)
          shown <- Request.view request
          Monad.guard (null (PlayerView.stack shown))
          case Text.unpack kind of
            "ChooseAction" -> fmap R.Number (Web.Question.passIndex request)
            "DeclareAttackers" -> Just (R.Array [])
            _ -> Nothing
    IORef.writeIORef seatRef state {paying = Maybe.isJust auto, skipping = if skipping state == turn then skipping state else Nothing}
    let seat = Question.seat $ Request.question request
        -- The board as it stands, while the seat is not asked: redrawn only
        -- when what it shows changed, not at every pass of priority.
        watch how reply = do
          traced how
          Monad.forM_ (Request.view request) $ \shown -> do
            now <- IORef.readIORef seatRef
            let seen = shown {PlayerView.priority = Nothing}
            Monad.when (watched now /= Just seen) $ do
              IORef.writeIORef seatRef now {watched = Just seen}
              Browser.display page . fmap Right $ Web.Question.watching request (Web.Question.blank {Web.Question.logged = loggedFor seat now, Web.Question.fresh = freshOf seat now, Web.Question.level = level now})
          pure reply
        conceded = if conceding state && kind == Text.pack "Concede" then Just (R.Text (Text.pack "Concedes")) else Nothing
    reply <- case (Monad.mplus conceded (Web.Question.automatic (level state) request), auto, skipped) of
      (Just reply, _, _) -> watch "answered unasked" reply
      (_, Just reply, _) -> watch "paid by auto" reply
      (_, _, Just reply) -> watch "skipped" reply
      _ -> do
        traced "asked"
        Monad.when (facing state /= Just seat) $ do
          Browser.display page . fmap Right $ Web.Question.handOff request
          untilReady
          IORef.modifyIORef' seatRef $ \s -> s {facing = Just seat}
        -- A tap made while the board was only being watched is dropped.
        Browser.flush page
        -- The question replaces the watched board, which is drawn afresh after.
        IORef.modifyIORef' seatRef $ \st -> st {watched = Nothing}
        answer request (Web.Question.initial request)
    returned <- Clock.getMonotonicTime
    IORef.modifyIORef' seatRef $ \s -> s {answeredAt = returned, decided = Map.insert seat (Seq.length (loggedFor seat s)) (decided s)}
    pure reply
  where
    untilReady = Browser.next page >>= \(event, _) -> Monad.unless (event == Right Web.Question.MkReady) untilReady
    answer pending picked = asking pending picked Nothing Nothing
    asking pending picked reading focus = do
      state <- IORef.readIORef seatRef
      let asked = Question.seat (Request.question pending)
          screen = Web.Question.MkScreen picked reading (loggedFor asked state) (freshOf asked state) focus (level state)
      Browser.display page . fmap Right $ Web.Question.view pending screen
      (event, value) <- Browser.next page
      case event of
        Right (Web.Question.MkRead object)
          | Just r <- readingOf pending object True -> asking pending picked (Just r) focus
        -- A preview shows only while no tapped sheet is open, and ends only
        -- itself.
        Right (Web.Question.MkPeek object)
          | maybe True (not . Web.Card.pinned) reading,
            Just r <- readingOf pending object False ->
              asking pending picked (Just r) focus
        Right Web.Question.MkUnpeek
          | maybe False (not . Web.Card.pinned) reading -> asking pending picked Nothing focus
        Right e -> case Web.Question.step pending picked e value of
          Web.Question.MkReplied answered -> do
            Monad.when (e == Web.Question.MkAuto) . IORef.modifyIORef' seatRef $ \s -> s {paying = True}
            Monad.when (e == Web.Question.MkSkip) . IORef.modifyIORef' seatRef $ \s -> s {skipping = fmap PlayerView.turn (Request.view pending)}
            Monad.when (e == Web.Question.MkConcede) . IORef.modifyIORef' seatRef $ \s -> s {conceding = True}
            pure answered
          -- A pick made from the card's sheet leaves the sheet open.
          Web.Question.MkPicking now -> do
            case e of
              Web.Question.MkLevel l -> IORef.modifyIORef' seatRef $ \st -> st {level = l}
              _ -> pure ()
            asking pending now (if isToggle e then reading else Nothing) (focusAfter e focus)
          Web.Question.MkRefused problem -> asking pending {Request.problem = Just problem} picked Nothing focus
        Left _ -> asking pending picked reading focus
    readingOf pending object pinned = do
      shown <- Request.view pending
      o <- Render.recalled shown object
      pure (Web.Card.MkReading object (either (pure . Text.pack . Exception.displayException) id (Web.Card.oracle lookUp o)) pinned)
    focusAfter e focus = case e of
      Web.Question.MkFocus g -> if focus == Just g then Nothing else Just g
      Web.Question.MkClear -> Nothing
      _ -> focus
    isToggle e = case e of
      Web.Question.MkToggle _ _ -> True
      Web.Question.MkFocus _ -> True
      _ -> False

-- | What the page keeps between request lines: the seat the screen last
-- showed, whether the seat chose auto for the payment under way, what the
-- seat has seen happen, and when it last answered.
data Seat = MkSeat
  { facing :: Maybe PlayerId.PlayerId,
    paying :: Bool,
    -- | How much the page answers for the seat (Web.Question.Level).
    level :: Web.Question.Level,
    -- | The turn the seat asked to pass to the end of (Web.Question.MkSkip).
    skipping :: Maybe Natural,
    -- | Whether the seat conceded (Web.Question.MkConcede).
    conceding :: Bool,
    -- | Each seat's life whenever it changed, beside the turn (Ending.trail).
    lives :: Seq.Seq (Natural, [(PlayerId.PlayerId, Integer)]),
    -- | The view last drawn while watching, less whose priority it was.
    watched :: Maybe PlayerView.PlayerView,
    -- | How far the seat has read each game's log (Log.unseen).
    marks :: Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural),
    -- | The entries each seat has read so far, oldest first: the seat the
    -- page plays's is the game log the end shows.
    logged :: Map.Map PlayerId.PlayerId (Seq.Seq Log.Entry),
    -- | How many entries each seat had logged when it last answered.
    decided :: Map.Map PlayerId.PlayerId Int,
    -- | When the last line was answered, the engine's work since then being
    -- what the trace times.
    answeredAt :: Double
  }

-- | The page's state before its first request line, facing the seat it
-- plays, at that auto-pass level.
newSeat :: Web.Question.Level -> IO Seat
newSeat initial = MkSeat (Just player) False initial Nothing False Seq.empty Nothing Map.empty Map.empty Map.empty <$> Clock.getMonotonicTime

-- | The entries a seat has read so far, oldest first.
loggedFor :: PlayerId.PlayerId -> Seat -> Seq.Seq Log.Entry
loggedFor seat state = Map.findWithDefault Seq.empty seat (logged state)

-- | How many entries a seat has logged since it last answered.
freshOf :: PlayerId.PlayerId -> Seat -> Natural
freshOf seat state = Int.toNaturalSaturating (Seq.length (loggedFor seat state) - Map.findWithDefault 0 seat (decided state))

-- | The page's state with the entries of the view its seat has not read yet
-- logged for that seat.
witness :: PlayerView.PlayerView -> Seat -> Seat
witness view state =
  let (moved, unread) = Log.unseen (marks state) view
      now = [(SeatView.player seat, SeatView.life seat) | seat <- PlayerView.seats view]
      changed = fmap snd (Seq.lookup (Seq.length (lives state) - 1) (lives state)) /= Just now
   in state {marks = moved, logged = Map.insertWith (flip (<>)) (PlayerView.viewer view) (Seq.fromList unread) (logged state), lives = if changed then lives state Seq.|> (PlayerView.turn view, now) else lives state}

message :: [Text.Text] -> Html.Html msg
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Html.text t] | t <- said]

-- | While the registry builds and the game deals: the match-up, and a spinner.
starting :: Text.Text -> Text.Text -> Html.Html msg
starting one two =
  Element.el
    "main"
    ["message", "starting"]
    []
    [ Element.el "div" ["spinner"] [Element.attr "aria-hidden" (Text.pack "true")] [],
      Element.el "p" [] [] [Element.string "Shuffling up\x2026"],
      Element.el "p" ["matchup-line"] [] [Html.text (Lobby.label one), Element.el "span" ["versus"] [] [Element.string " vs "], Html.text (Lobby.label two)]
    ]
