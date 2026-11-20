-- | A game against one or more random bots in the browser: `parun play
-- --json`'s game with every request line answered on the page
-- (Parun.Web.Question), ending on how it ended (Parun.Web.Ending).
module Parun.Browser.Hotseat where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Key as Key
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Screen as Screen

-- | What the player chose on leaving a game for the home screen.
data Leave
  = -- | Back to the question pending.
    MkResume
  | -- | The game given up for another.
    MkAbandon
  deriving (Eq, Show)

-- | How a game the page played came out.
data Played
  = MkFinished Ending.Ending
  | -- | Given up from the home screen for another game, so no ending shows.
    MkAbandoned
  deriving (Eq, Show)

-- | Unwinds a game given up from the home screen.
data Abandoned = MkAbandonedGame
  deriving (Show)

instance Exception.Exception Abandoned

-- | A game between these decks (name and file, the player's first) on the
-- page, from this seed or a fresh one, every request line answered there
-- (Parun.Web.Question) at the auto-pass level kept in `levelRef`, and
-- left with the level last chosen there; leaving for the home screen
-- through `leave`: how it came out, or why it could not start.
game :: Browser.Page Screen.Event -> Registry.Registry Identity.Identity -> IORef.IORef Web.Question.Level -> NonEmpty.NonEmpty (Text.Text, FilePath) -> Maybe Key.Key -> IO Leave -> IO (Either [Text.Text] Played)
game page registry levelRef seats given leave = do
  seatRef <- IORef.newIORef . newSeat =<< IORef.readIORef levelRef
  keepingLevel levelRef seatRef . Exception.handle (\MkAbandonedGame -> pure (Right MkAbandoned)) $ do
    listed <- traverse (Command.load registry . snd) seats
    case sequenceA listed of
      Left _ -> pure (Left (foldMap (Either.fromLeft []) listed))
      Right lists -> do
        settled <- Command.settle Nothing (zip (fmap snd (NonEmpty.toList seats)) (NonEmpty.toList lists))
        case settled of
          Left problems -> pure (Left problems)
          Right format -> do
            seed <- maybe Key.fresh pure given
            (outcome, recorded) <- Command.game (exchange (Identity.runIdentity . Interpreter.chosenCard registry) page seatRef leave) (options (Natural.length seats)) registry format (fmap DeckList.deck lists) seed
            -- What the player saw after their last decision: the killing blow, say.
            Monad.forM_ outcome $ IORef.modifyIORef' seatRef . witness . PlayerView.Engine.viewFor player . snd
            finished <- IORef.readIORef seatRef
            let names = fmap fst seats
            pure . Right . MkFinished $ Ending.MkEnding (verdictOf outcome) (either (\reason -> [Text.pack "The game stopped: " <> reason <> Text.pack "."]) (\(result, gs) -> Render.result result : Render.departures gs) outcome) (Command.rendered recorded) (loggedFor player finished) player (NonEmpty.head names, NonEmpty.tail names) (Foldable.toList (lives finished)) (Key.toHex seed)

-- | Runs a game, then keeps the auto-pass level its seat ended on.
keepingLevel :: IORef.IORef Web.Question.Level -> IORef.IORef Seat -> IO a -> IO a
keepingLevel levelRef seatRef run = run `Exception.finally` (IORef.readIORef seatRef >>= IORef.writeIORef levelRef . level)

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

-- | The player is seat 1, over the line protocol, of so many seats; the
-- random bot is every other.
options :: Natural -> PlayOptions.PlayOptions
options count = PlayOptions.none {PlayOptions.json = True, PlayOptions.bots = Set.fromList [2 .. count]}

-- | One request answered on the page, as values (Protocol.Exchange): a reply the seat is never asked
-- for, or auto's next source once the seat chose auto for this payment, at
-- once; else after a hand-off when the screen last faced another seat, the
-- question's form until it answers, a card tapped showing its text from the
-- card data through `lookUp`, and the home screen through `leave` while
-- the seat is asked.
exchange :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page Screen.Event -> IORef.IORef Seat -> IO Leave -> Protocol.Exchange IO
exchange lookUp page seatRef leave request = do
  fmap (Just . Right) $ do
    Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
    state <- IORef.readIORef seatRef
    let auto = if paying state || level state == Web.Question.MkAutoPass then Web.Question.paying request else Nothing
        kind = Question.kind $ Request.question request
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
        -- when what it shows changed, not at every pass of priority, and
        -- only while the screen faces the seat, else what it last showed stays.
        watch reply = do
          Monad.forM_ (Request.view request) $ \shown -> do
            now <- IORef.readIORef seatRef
            let seen = shown {PlayerView.priority = Nothing}
            Monad.when (watched now /= Just seen) . Monad.forM_ (Web.Question.watching (facing now) request (Web.Question.blank {Web.Question.logged = loggedFor seat now, Web.Question.fresh = freshOf seat now, Web.Question.level = level now})) $ \board -> do
              IORef.writeIORef seatRef now {watched = Just seen}
              Browser.display page (fmap Screen.MkQuestion board)
          pure reply
        conceded = if conceding state && kind == Text.pack "Concede" then Just (R.Text (Text.pack "Concedes")) else Nothing
    reply <- case (Monad.mplus conceded (Web.Question.automatic (level state) request), auto, skipped) of
      (Just reply, _, _) -> watch reply
      (_, Just reply, _) -> watch reply
      (_, _, Just reply) -> watch reply
      _ -> do
        Monad.when (facing state /= Just seat) $ do
          Browser.display page . fmap Screen.MkQuestion $ Web.Question.handOff request
          untilReady
          IORef.modifyIORef' seatRef $ \s -> s {facing = Just seat}
        -- A tap made while the board was only being watched is dropped.
        Browser.flush page
        -- The question replaces the watched board, which is drawn afresh after.
        IORef.modifyIORef' seatRef $ \st -> st {watched = Nothing}
        answer request (Web.Question.initial request)
    IORef.modifyIORef' seatRef $ \s -> s {decided = Map.insert seat (Seq.length (loggedFor seat s)) (decided s)}
    pure reply
  where
    untilReady = Browser.next page >>= \(event, _) -> Monad.unless (event == Screen.MkQuestion Web.Question.MkReady) untilReady
    answer pending picked = asking pending picked Nothing Nothing
    asking pending picked reading focus = do
      state <- IORef.readIORef seatRef
      let asked = Question.seat (Request.question pending)
          screen = Web.Question.MkScreen picked reading (loggedFor asked state) (freshOf asked state) focus (level state)
      Browser.display page . fmap Screen.MkQuestion $ Web.Question.view pending screen
      (event, value) <- Browser.next page
      case event of
        Screen.MkQuestion (Web.Question.MkRead object)
          | Just r <- readingOf pending object True -> asking pending picked (Just r) focus
        -- A preview shows only while no tapped sheet is open, and ends only
        -- itself.
        Screen.MkQuestion (Web.Question.MkPeek object)
          | maybe True (not . Web.Card.pinned) reading,
            Just r <- readingOf pending object False ->
              asking pending picked (Just r) focus
        Screen.MkQuestion Web.Question.MkUnpeek
          | maybe False (not . Web.Card.pinned) reading -> asking pending picked Nothing focus
        -- The home screen runs here, the question waiting; giving the game
        -- up unwinds it (game).
        Screen.MkQuestion Web.Question.MkHome -> do
          left <- leave
          case left of
            MkResume -> asking pending picked reading focus
            MkAbandon -> Exception.throwIO MkAbandonedGame
        Screen.MkQuestion e -> case Web.Question.step pending picked e value of
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
        _ -> asking pending picked reading focus
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
    decided :: Map.Map PlayerId.PlayerId Int
  }

-- | The page's state before its first request line, facing the seat it
-- plays, at that auto-pass level.
newSeat :: Web.Question.Level -> Seat
newSeat initial = MkSeat (Just player) False initial Nothing False Seq.empty Nothing Map.empty Map.empty Map.empty

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
starting :: NonEmpty.NonEmpty Text.Text -> Html.Html msg
starting decks =
  Element.el
    "main"
    ["message", "starting"]
    []
    [ Element.el "div" ["spinner"] [Element.attr "aria-hidden" (Text.pack "true")] [],
      Element.el "p" [] [] [Element.string "Shuffling up\x2026"],
      Element.el "p" ["matchup-line"] [] (List.intersperse (Element.el "span" ["versus"] [] [Element.string " vs "]) (fmap (Html.text . Lobby.label) (NonEmpty.toList decks)))
    ]
