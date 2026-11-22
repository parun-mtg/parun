-- | A game against one or more search bots in the browser: `parun play
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
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
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

-- | Unwinds a game given up from the home screen.
data Abandoned = MkAbandonedGame
  deriving (Show)

instance Exception.Exception Abandoned

-- | A game between these decks, each a label and its list's text (the
-- player's first), on the page, shuffled by `seed`: every request line of the
-- player's seat answered there (Parun.Web.Question) at the auto-pass level
-- kept in `levelRef`, and left with the level last chosen there; each seat
-- in `remotes` answered by its own exchange (Parun.Web.Link.remote), and
-- every other by the search bot at the difficulty and persona `bots` names;
-- leaving for the home screen through
-- `leave`. The game picks up after the replies in `stored`, each made again
-- (Protocol.resumed) and each bot's given back to it (Game.remembered), and
-- every reply made from there, a bot's too, is given to `kept`. Each bot
-- deals what it cannot see from the game's format's pool among `cards`
-- (Command.pooled), by the legality table `table` gives.
-- Once over, `tell` is given each seat's verdict, what is said of the game
-- to each seat and its scenario file: how it came out, or why it could not
-- start.
game :: Browser.Page Screen.Event -> Registry.Registry Identity.Identity -> [Either Text.Text Card.Card] -> IO (Either Text.Text LegalityTable.LegalityTable) -> IORef.IORef Web.Question.Level -> (Difficulty.Difficulty, Persona.Persona) -> NonEmpty.NonEmpty (FilePath, Text.Text) -> Map.Map PlayerId.PlayerId (Protocol.Exchange IO) -> Key.Type.Key -> [R.Reply] -> (R.Reply -> IO ()) -> ((PlayerId.PlayerId -> Ending.Verdict) -> (PlayerId.PlayerId -> [Text.Text]) -> Text.Text -> IO ()) -> (Maybe PlayerView.PlayerView -> IO Leave) -> IO (Either [Text.Text] Ending.Played)
game page registry cards table levelRef bots seats remotes seed stored kept tell leave = do
  seatRef <- IORef.newIORef . newSeat player =<< IORef.readIORef levelRef
  keepingLevel levelRef seatRef . Exception.handle (\MkAbandonedGame -> pure (Right Ending.MkAbandoned)) $ do
    let listed = fmap (uncurry (Command.loadText registry)) seats
    case sequenceA listed of
      Left _ -> pure (Left (foldMap (Either.fromLeft []) listed))
      Right lists -> do
        settled <- Command.settle Nothing (zip (fmap fst (NonEmpty.toList seats)) (NonEmpty.toList lists))
        prior <- either (pure . Left) (\format -> Command.pooled table format cards) settled
        case (,) <$> settled <*> prior of
          Left problems -> pure (Left problems)
          Right (format, pooled) -> do
            storedRef <- IORef.newIORef stored
            let here = exchange (Identity.runIdentity . Interpreter.chosenCard registry) page seatRef leave
                next = IORef.atomicModifyIORef' storedRef $ \left -> maybe ([], Nothing) (\(reply, rest) -> (rest, Just reply)) (List.uncons left)
                -- What a seat this page plays sees as its reply is made again.
                replayed request = Monad.when (Map.notMember (Question.seat (Request.question request)) remotes) . Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
            (outcome, recorded) <- Command.game (Protocol.resumed next replayed kept (Protocol.bySeat remotes here)) next kept (options bots (Natural.length seats) (Map.keysSet remotes)) pooled registry format (fmap DeckList.deck lists) seed
            -- What the player saw after their last decision: the killing blow, say.
            Monad.forM_ outcome $ IORef.modifyIORef' seatRef . witness . PlayerView.Engine.viewFor player . snd
            finished <- IORef.readIORef seatRef
            let said seat = either (\reason -> [Text.pack "The game stopped: " <> stopped reason <> Text.pack "."]) (\(result, gs) -> Wording.result (Just seat) result : Wording.departures (Just seat) gs) outcome
                -- A remote seat's exchange ends only when its channel closes.
                stopped reason = if not (Map.null remotes) && reason == Text.pack "end of input" then Text.pack "a player's connection closed" else reason
                scenario = Command.rendered recorded
            tell (`verdictOf` outcome) said scenario
            pure . Right . Ending.MkFinished $ Ending.MkEnding (verdictOf player outcome) (said player) scenario (loggedFor player finished) player (Foldable.toList (lives finished))

-- | Runs a game, then keeps the auto-pass level its seat ended on.
keepingLevel :: IORef.IORef Web.Question.Level -> IORef.IORef Seat -> IO a -> IO a
keepingLevel levelRef seatRef run = run `Exception.finally` (IORef.readIORef seatRef >>= IORef.writeIORef levelRef . level)

-- | How the game ended for this seat.
verdictOf :: PlayerId.PlayerId -> Either a (Result.Result, b) -> Ending.Verdict
verdictOf seat outcome = case outcome of
  Left _ -> Ending.MkStopped
  Right (Result.Won pid, _) -> if pid == seat then Ending.MkWon else Ending.MkLost
  Right (Result.TeamWon _, _) -> Ending.MkStopped
  Right (Result.Drawn, _) -> Ending.MkDrawn

-- | The seat the page plays.
player :: PlayerId.PlayerId
player = PlayerId.MkPlayerId 0

-- | Of so many seats, the player's (seat 1) and each remote one speak the
-- line protocol; the search bot plays every other, at this difficulty and
-- persona (Parun.Play.Opponent).
options :: (Difficulty.Difficulty, Persona.Persona) -> Natural -> Set.Set PlayerId.PlayerId -> PlayOptions.PlayOptions
options (difficulty, persona) count remote =
  PlayOptions.none
    { PlayOptions.json = True,
      PlayOptions.bots = Set.fromList [n | n <- [2 .. count], Set.notMember (Command.idOf n) remote],
      PlayOptions.budget = Opponent.budgetAt difficulty (PlayOptions.budget PlayOptions.none),
      PlayOptions.weights = Opponent.weightsOf persona
    }

-- | One request answered on the page, as values (Protocol.Exchange): a reply the seat is never asked
-- for, or auto's next source once the seat chose auto for this payment, at
-- once; else after a hand-off when the screen last faced another seat, the
-- question's form until it answers, a card tapped showing its text from the
-- card data through `lookUp`, and the home screen through `leave` while
-- the seat is asked, given the board the question shows.
exchange :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page Screen.Event -> IORef.IORef Seat -> (Maybe PlayerView.PlayerView -> IO Leave) -> Protocol.Exchange IO
exchange lookUp page seatRef leave request = do
  fmap (Just . Right) $ do
    Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
    state <- IORef.readIORef seatRef
    let auto = if paying state || level state == Web.Question.MkAutoPass then Web.Question.paying request else Nothing
        kind = Question.kind $ Request.question request
        -- The seat's own stop (Web.Question.passesFor).
        (skipped, kept) = maybe (Nothing, Nothing) (`Web.Question.passesFor` request) (skipping state)
    IORef.writeIORef seatRef state {paying = Maybe.isJust auto, skipping = kept}
    let seat = Question.seat $ Request.question request
        -- The board as it stands, while the seat is not asked: redrawn only
        -- when what it shows changed, not at every pass of priority, and
        -- only while the screen faces the seat, else what it last showed stays.
        watch reply = do
          keepLevel
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
        -- A tap made while the board was only being watched is dropped,
        -- but for the auto-pass level.
        keepLevel
        -- The question replaces the watched board, which is drawn afresh after.
        IORef.modifyIORef' seatRef $ \st -> st {watched = Nothing}
        answer request (Web.Question.initial request)
    IORef.modifyIORef' seatRef $ \s -> s {decided = Map.insert seat (Seq.length (loggedFor seat s)) (decided s)}
    pure reply
  where
    -- The last auto-pass level tapped while the seat watched, kept, and the
    -- watched board drawn afresh to show it; every other tap dropped.
    keepLevel = do
      dropped <- Browser.flush page
      Monad.forM_ (Maybe.listToMaybe (reverse [l | Screen.MkQuestion (Web.Question.MkLevel l) <- dropped])) $ \l ->
        IORef.modifyIORef' seatRef $ \st -> st {level = l, watched = Nothing}
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
        -- itself, never the tapped sheet (the pointer leaving the tile for
        -- that sheet's scrim ends a preview).
        Screen.MkQuestion (Web.Question.MkPeek object)
          | maybe True (not . Web.Card.pinned) reading,
            Just r <- readingOf pending object False ->
              asking pending picked (Just r) focus
          | otherwise -> asking pending picked reading focus
        Screen.MkQuestion Web.Question.MkUnpeek
          | maybe False (not . Web.Card.pinned) reading -> asking pending picked Nothing focus
          | otherwise -> asking pending picked reading focus
        -- The home screen runs here, the question waiting; giving the game
        -- up unwinds it (game).
        Screen.MkQuestion Web.Question.MkHome -> do
          left <- leave (Request.view pending)
          case left of
            MkResume -> asking pending picked reading focus
            MkAbandon -> Exception.throwIO MkAbandonedGame
        Screen.MkQuestion e -> case Web.Question.step pending picked e value of
          Web.Question.MkReplied answered -> do
            Monad.when (e == Web.Question.MkAuto) . IORef.modifyIORef' seatRef $ \s -> s {paying = True}
            case e of
              Web.Question.MkSkip stop -> IORef.modifyIORef' seatRef $ \s -> s {skipping = Just stop}
              _ -> pure ()
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
      o <- Wording.recalled shown object
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
    -- | How far the seat asked to pass (Web.Question.MkSkip).
    skipping :: Maybe Web.Question.Until,
    -- | Whether the seat conceded (Web.Question.MkConcede).
    conceding :: Bool,
    -- | Each seat's life whenever it or the turn changed (Ending.sampled).
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
newSeat :: PlayerId.PlayerId -> Web.Question.Level -> Seat
newSeat seat initial = MkSeat (Just seat) False initial Nothing False Seq.empty Nothing Map.empty Map.empty Map.empty

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
   in state {marks = moved, logged = Map.insertWith (flip (<>)) (PlayerView.viewer view) (Seq.fromList unread) (logged state), lives = Ending.sampled (PlayerView.turn view) now (lives state)}

message :: [Text.Text] -> Html.Html msg
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Html.text t] | t <- said]

-- | While the registry builds and the game deals: the match-up, each seat's
-- deck by its name and list, and a spinner.
starting :: NonEmpty.NonEmpty (Text.Text, Text.Text) -> Html.Html msg
starting decks =
  Element.el
    "main"
    ["message", "starting"]
    []
    [ Element.el "div" ["spinner"] [Element.attr "aria-hidden" (Text.pack "true")] [],
      Element.el "p" [] [] [Element.string "Shuffling up\x2026"],
      Element.el "p" ["matchup-line"] [] (List.intersperse (Element.el "span" ["versus"] [] [Element.string " vs "]) (fmap (\deck -> Html.text (DeckBrowser.label (DeckBrowser.titleIn deck) (fst deck))) (NonEmpty.toList decks)))
    ]
