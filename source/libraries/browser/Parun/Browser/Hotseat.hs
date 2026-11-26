-- | A game in the browser, dealt from decks or played on from a replay's
-- position: `parun play --json`'s game with every request line answered on
-- the page (Parun.Web.Question), ending on how it ended (Parun.Web.Ending).
module Parun.Browser.Hotseat where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
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
import qualified GHC.Clock as Clock
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.Start as Start
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Clock as Web.Clock
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Link as Link
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

-- | Unwinds a game given up from the home screen.
data Abandoned = MkAbandonedGame
  deriving (Show)

instance Exception.Exception Abandoned

-- | Where a game starts.
data Start
  = -- | Dealt from these decks, each a label and its list's text, seat 0's
    -- first, with these options (CR 800.2) under the format the lists'
    -- headers name (Command.game).
    MkDealt GameSettings.GameSettings (NonEmpty.NonEmpty (FilePath, Text.Text))
  | -- | Played on from this position of the scenario (Command.branch).
    MkBranched Scenario.Type.Scenario Natural

-- | A game from `start` on the page, shuffled by `seed`: every request line
-- of the seat `host` answered there (Parun.Web.Question) at the auto-pass
-- level kept in `levelRef`, and left with the level last chosen there; each
-- seat in `remotes` answered by its own exchange (Parun.Web.Link.remote),
-- given what the host sent that seat before the request pending, the time
-- banks (Web.Clock.banks) as they read and what that seat conceding does,
-- and heard over the channel its entry in `heeds` numbers while another
-- seat decides, that entry reading whether it conceded, each
-- in `locals` on the page as the host's is, handed the screen first
-- (Web.Question.handOff) unless the host plays it too (Link.MkYours),
-- each in `goldfish` by the goldfish (Game.goldfish), and every other by
-- the search bot at the difficulty and persona its slot has in `bots`
-- (Lobby.botAt), every seat a person plays given `clock`'s minutes, if
-- timed, a seat out of time losing (Web.Clock.timed), as does a seat
-- conceding where that decides the game (Web.Question.decisive), which ends
-- it at once (Command.MkConceded), and a seat out of time at its priority,
-- or conceding at any seat's, in a game of three or more dealt here, which
-- leaves it at once, the rest playing on (Command.MkLeaving), else at its
-- next concession question, the page live while a
-- remote seat decides (awaiting) or a search bot does (breathing); leaving
-- for the home screen through `leave`. The game picks up after the replies in
-- `stored`, each made again (Protocol.resumed) and each bot's given back
-- to it (Game.remembered), its clocks as the last reply left them, the
-- clock `running` as the page was left charged (Web.Clock.resumed), and
-- every reply made from there, a bot's too, is given to `kept` with the
-- time banks then, each made since a seat's concession question taken back
-- through `retract` as it leaves at a priority, and each clock as it starts
-- to `keepRunning` (Web.Clock.keeping); the seat each prompt asks, a
-- bot's too, is given to `decides` first, with the game then, its depth
-- (Link.deciding) and the time banks. Each bot deals what it cannot see from the game's
-- format's pool among `cards` (Command.pooled), by the legality table `table` gives.
-- Once over, `tell` is given each seat's ending, a guest's log and life
-- trail built from what its seat was sent (Link.seeing), which its page
-- loses to a reload: how it came out, or why it could not start, taking
-- no seat's side where another person plays at this screen
-- (Link.MkHandedOver).
game :: Browser.Page Screen.Event -> Registry.Registry Identity.Identity -> [Either Text.Text Card.Card] -> IO (Either Text.Text LegalityTable.LegalityTable) -> IORef.IORef Web.Question.Level -> Map.Map Int (Difficulty.Difficulty, Persona.Persona) -> Start -> Maybe Natural -> PlayerId.PlayerId -> Set.Set PlayerId.PlayerId -> Map.Map PlayerId.PlayerId (IO Ending.Seen -> IO Link.Banks -> IO (Maybe (Either Text.Text R.Reply)) -> Protocol.Exchange IO) -> Map.Map PlayerId.PlayerId (Int, IO Bool) -> Map.Map PlayerId.PlayerId Link.Sitting -> Key.Type.Key -> [(R.Reply, Link.Banks)] -> Maybe Web.Clock.Kept -> (R.Reply -> Link.Banks -> IO ()) -> IO () -> (Web.Clock.Kept -> IO ()) -> (PlayerId.PlayerId -> GameState.GameState -> Natural -> Link.Banks -> IO ()) -> ((PlayerId.PlayerId -> Ending.Ending) -> IO ()) -> (Maybe PlayerView.PlayerView -> IO Leave) -> IO (Either [Text.Text] Ending.Played)
game page registry cards table levelRef bots start clock host goldfish remotes heeds locals seed stored running kept retract keepRunning decides tell leave = do
  let people = Set.insert host (Map.keysSet remotes <> Map.keysSet locals)
      count = case start of
        MkDealt _ seats -> Natural.length seats
        MkBranched scenario _ -> Natural.length (Scenario.seatsOf scenario)
      options = (Lobby.playOptions bots count host (people <> goldfish)) {PlayOptions.goldfish = Set.map ((+ 1) . PlayerId.unwrap) goldfish}
  seatRef <- IORef.newIORef . (\s -> s {yours = Set.insert host (Map.keysSet (Map.filter (== Link.MkYours) locals)), handed = Link.MkHandedOver `elem` locals, clocks = Web.Clock.resumed (Web.Clock.starting clock people) (fmap snd stored) running}) . newSeat host Ending.unseen =<< IORef.readIORef levelRef
  keepingLevel levelRef seatRef . Exception.handle (\MkAbandonedGame -> pure (Right Ending.MkAbandoned)) $ do
    -- The game's format, and how it is played from there: its outcome and
    -- the scenario a game dealt here replays as.
    ready <- case start of
      MkDealt settings seats -> do
        let listed = fmap (uncurry (Command.loadText registry)) seats
        case sequenceA listed of
          Left _ -> pure (Left (foldMap (Either.fromLeft []) listed))
          Right lists -> do
            settled <- Command.settle settings Nothing (zip (fmap fst (NonEmpty.toList seats)) (NonEmpty.toList lists))
            pure . flip fmap settled $ \format -> (format, \send next keep watchers breathe pooled -> fmap (fmap Command.rendered) (Command.game (Text.pack "the web client") send next keep watchers breathe options pooled registry settings {GameSettings.format = format} (fmap DeckList.deck lists) seed))
      MkBranched scenario at ->
        let format = case Scenario.Type.start scenario of
              Start.FromDecks settings _ _ -> GameSettings.format settings
              Start.FromBoard _ -> Nothing
         in pure (Right (format, \send next keep watchers breathe pooled -> fmap (\outcome -> (outcome, Text.empty)) (Command.branch send next keep watchers breathe options pooled registry scenario at seed)))
    prior <- either (pure . Left) (\(format, _) -> Command.pooled table format cards) ready
    case (,) <$> ready <*> prior of
      Left problems -> pure (Left problems)
      Right ((_, played), pooled) -> do
        storedRef <- IORef.newIORef (fmap fst stored)
        -- What each guest's seat was sent.
        guestsRef <- IORef.newIORef (Ending.unseen <$ remotes)
        breathRef <- IORef.newIORef Nothing
        botReading <- IORef.newIORef Nothing
        -- The seat that conceded where that decided the game.
        concededRef <- IORef.newIORef Nothing
        -- The seats that left the game at a priority (leaveNow), or leave
        -- at their next concession question (concedeHeard).
        leftRef <- IORef.newIORef Set.empty
        -- The request pending, and the seats asked the main game's
        -- concession questions just before it (Command.concessionRun).
        pendingRef <- IORef.newIORef Nothing
        runRef <- IORef.newIORef ([], False)
        let lookUp = Identity.runIdentity . Interpreter.chosenCard registry
            -- Each seat a person plays on its clock, if timed, kept as
            -- it starts for a reload to charge.
            timed seat ask = Web.Clock.timed Clock.getMonotonicTime (Browser.within page) (clocks <$> IORef.readIORef seatRef) (\f -> IORef.modifyIORef' seatRef (\s -> s {clocks = f (clocks s)})) (expire seat) seat $ \request -> do
              at <- Clock.getMonotonicTime
              (epoch, _) <- Browser.clock
              Monad.mapM_ keepRunning . Web.Clock.keeping at epoch . clocks =<< IORef.readIORef seatRef
              ask request
            banks = Web.Clock.banks <$> Clock.getMonotonicTime <*> (clocks <$> IORef.readIORef seatRef)
            keep reply = banks >>= kept reply
            -- A seat conceding where that decides the game ends it at once.
            concedeAs :: PlayerId.PlayerId -> IO a
            concedeAs seat = IORef.writeIORef concededRef (Just seat) >> Exception.throwIO (Command.MkConceded seat)
            -- A game dealt here asks a seat leaving at a priority its
            -- concession question again (Command.rewinding), and each
            -- asked after it, the replies kept since, its own among them,
            -- taken back for those made then ('departing'); a goldfish's
            -- are never kept.
            rewinds = case start of
              MkDealt {} -> True
              MkBranched {} -> False
            leaveNow seat = do
              -- Its clock stops, an exchange given up answering nothing.
              at <- Clock.getMonotonicTime
              IORef.modifyIORef' seatRef (\s -> s {clocks = Web.Clock.stop at (clocks s)})
              IORef.modifyIORef' leftRef (Set.insert seat)
              (run, _) <- IORef.readIORef runRef
              Monad.replicateM_ (length (filter (`Set.notMember` goldfish) (dropWhile (/= seat) run))) retract
              Exception.throwIO (Command.MkLeaving seat)
            -- A seat conceding while `request` is pending leaves at once
            -- where that decides the game or the game rewinds at a
            -- priority, else not (False).
            concedeAt request seat
              | Web.Question.decisive request = concedeAs seat
              | rewinds && Web.Question.atPriority request = leaveNow seat
              | otherwise = pure False
            -- A seat conceding while another decides, likewise, else
            -- leaving at its next concession question; once only.
            concedeHeard seat = do
              already <- Set.member seat <$> IORef.readIORef leftRef
              Monad.unless already $ do
                pending <- IORef.readIORef pendingRef
                left <- maybe (pure False) (`concedeAt` seat) pending
                Monad.unless left (IORef.modifyIORef' leftRef (Set.insert seat))
            -- Each guest heard while another seat decides.
            hearingGuests play = foldr (\(seat, (n, heard)) -> Browser.hearing page n (heard >>= \said -> Monad.when said (concedeHeard seat))) play (Map.toList heeds)
            -- Each guest's own exchange reads its channel.
            deafTo seat = maybe id (Browser.deaf page . fst) (Map.lookup seat heeds)
            -- The request each prompt makes, and the concession questions
            -- asked just before it.
            watchPending =
              Observer.MkObserver
                { Observer.before = \asked -> Game.underneath (IORef.writeIORef pendingRef (Protocol.requestFor asked) >> IORef.modifyIORef' runRef (Command.concessionRun asked)),
                  Observer.after = \_ _ -> pure ()
                }
            -- Out of time in a game of three or more, likewise.
            expire seat request = Monad.when (rewinds && Web.Question.atPriority request && not (Web.Question.decisive request)) (leaveNow seat)
            -- A seat that left, or leaves at its next concession question,
            -- answers it yes, off its clock; a guest, whose concession is
            -- heard at once, is asked it over no channel.
            departing live request = do
              left <- IORef.readIORef leftRef
              let question = Request.question request
                  seat = Question.seat question
                  concession = Question.kind question == Text.pack "Concede"
              case () of
                _
                  | concession && Set.member seat left -> pure (Just (Right (R.Text (Text.pack "Concedes"))))
                  | concession && Map.member seat remotes -> pure (Just (Right (R.Text (Text.pack "Continues"))))
                  | otherwise -> live request
            here request = timed (Question.seat (Request.question request)) (exchange lookUp page seatRef leave (concedeAt request)) request
            -- The page stays live while another browser's seat decides,
            -- and while a search bot does; a guest conceding at its own
            -- question leaves at once where it can, else at its next
            -- concession question, its pass read.
            remoteConceded request seat = Nothing <$ (concedeAt request seat >>= \left -> Monad.unless left (IORef.modifyIORef' leftRef (Set.insert seat)))
            elsewhere = Map.mapWithKey (\seat ask -> timed seat (\request -> deafTo seat (awaiting lookUp page seatRef leave concedeHeard (const [seat]) (Link.seeing (sentTo seat) (ask (Map.findWithDefault Ending.unseen seat <$> IORef.readIORef guestsRef) banks (remoteConceded request seat)) request)))) remotes
            sentTo seat shown = IORef.modifyIORef' guestsRef (Map.adjust (Ending.see shown) seat)
            next = IORef.atomicModifyIORef' storedRef $ \left -> maybe ([], Nothing) (\(reply, rest) -> (rest, Just reply)) (List.uncons left)
            -- What a seat sees as its reply is made again, and what a
            -- seat this page plays was asked.
            replayed request
              | Map.member (Question.seat (Request.question request)) remotes = Monad.forM_ (Request.view request) (sentTo (Question.seat (Request.question request)))
              | otherwise = do
                  Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
                  IORef.modifyIORef' seatRef (keepAsked request)
            -- Each seat this page plays sees the game move on while
            -- another browser's seat decides (awaiting).
            movedOn seat gs depth = do
              moving seatRef breathRef botReading (host : Map.keys locals) (\s -> Map.member (PlayerId.unwrap s + 1) (PlayOptions.bots options)) seat gs depth
              banks >>= decides seat gs depth
        (outcome, scenario) <- hearingGuests (played (Protocol.resumed next replayed keep (departing (Protocol.bySeat elsewhere here))) next keep [Link.deciding movedOn, watchPending] (breathing lookUp page seatRef leave concedeHeard botReading breathRef) pooled)
        finished <- IORef.readIORef seatRef
        sent <- IORef.readIORef guestsRef
        conceded <- IORef.readIORef concededRef
        let -- A seat out of time, or conceding where that decides the
            -- game, ends it as it left.
            early = Monad.mplus (fmap (\seat -> (seat, Text.pack "ran out of time")) (Web.Clock.out (clocks finished))) (fmap (\seat -> (seat, Text.pack "conceded")) conceded)
            leaving reader = fmap (\(seat, why) -> Ending.leftEarly count why seat reader) early
            said reader = maybe (either (\reason -> [Text.pack "The game stopped: " <> stopped reason <> Text.pack "."]) (\(result, gs) -> Wording.result reader result : Wording.departures reader gs) outcome) snd (leaving reader)
            -- A remote seat's exchange ends only when its channel closes.
            stopped reason = if not (Map.null remotes) && reason == Text.pack "end of input" then Text.pack "a player's connection closed" else reason
            -- Told to `reader`, or to no seat in particular.
            endingAs reader seat = Link.ending (maybe (Ending.verdictOf reader outcome) fst (leaving reader)) (said reader) scenario seat (Maybe.isNothing reader) outcome (Map.findWithDefault (sighted finished) seat sent)
            endingFor seat = endingAs (Just seat) seat
            -- Another person plays at this screen, so its ending takes
            -- no seat's side.
            shared = Link.MkHandedOver `elem` locals
        tell endingFor
        pure . Right . Ending.MkFinished $ endingAs (if shared then Nothing else Just host) host

-- | The game come to `seat`'s decision, as it then is at this depth
-- (Link.deciding): each of `mine`, the seats this page plays, sees it stand
-- so, a seat `bot` names is timed for breathing, and a card read while bots
-- decided stays open until another seat is asked.
moving :: IORef.IORef Seat -> IORef.IORef (Maybe Breath) -> IORef.IORef (Maybe Web.Card.Reading) -> [PlayerId.PlayerId] -> (PlayerId.PlayerId -> Bool) -> PlayerId.PlayerId -> GameState.GameState -> Natural -> IO ()
moving seatRef breathRef botReading mine bot seat gs depth = do
  IORef.modifyIORef' seatRef (\s -> foldr (\one -> stand one (PlayerView.Engine.viewFor one gs) depth) s mine)
  asked <- Clock.getMonotonicTime
  IORef.writeIORef breathRef (if bot seat then Just (MkBreath seat asked False) else Nothing)
  Monad.unless (bot seat) (IORef.writeIORef botReading Nothing)

-- | Runs a game, then keeps the auto-pass level its seat ended on.
keepingLevel :: IORef.IORef Web.Question.Level -> IORef.IORef Seat -> IO a -> IO a
keepingLevel levelRef seatRef run = run `Exception.finally` (IORef.readIORef seatRef >>= IORef.writeIORef levelRef . level)

-- | One request answered on the page, as values (Protocol.Exchange): a reply the seat is never asked
-- for, or auto's next source or its mana once the seat chose auto for this payment, at
-- once; else after a hand-off when the screen last faced another seat but
-- for one the same person plays (Web.Question.handsOver), the
-- question's form until it answers, a card tapped showing its text from the
-- card data through `lookUp`, and the home screen through `leave` while
-- the seat is asked, given the board the question shows. A concession that
-- leaves at once where it can (Web.Question.MkConceded) goes to `concede`,
-- and answers nothing where it did, else passes.
exchange :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page Screen.Event -> IORef.IORef Seat -> (Maybe PlayerView.PlayerView -> IO Leave) -> (PlayerId.PlayerId -> IO Bool) -> Protocol.Exchange IO
exchange lookUp page seatRef leave concede request = do
  fmap (fmap Right) $ do
    Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
    IORef.modifyIORef' seatRef (keepAsked request)
    state <- IORef.readIORef seatRef
    let seat = Question.seat $ Request.question request
        auto = if Map.member seat (paying state) || level state == Web.Question.MkAutoPass then Web.Question.paying (Map.lookup seat (paying state)) request else Nothing
        kind = Question.kind $ Request.question request
        -- The seat's own stop (Web.Question.passesFor).
        (skipped, kept) = maybe (Nothing, Nothing) (`Web.Question.passesFor` request) (Map.lookup seat (skipping state))
        -- Auto's source question, kept for the mana its source then makes.
        paid
          | Maybe.isNothing auto = Map.delete seat
          | kind == Text.pack "ChooseManaSource" = Map.insert seat request
          | otherwise = id
    IORef.writeIORef seatRef state {paying = paid (paying state), skipping = Map.alter (const kept) seat (skipping state)}
    let -- The board as it stands, while the seat is not asked: redrawn only
        -- when what it shows changed, not at every pass of priority, and
        -- only while the screen faces the seat, or one the same person plays,
        -- else what it last showed stays.
        watch reply = do
          keepLevel
          Monad.forM_ (Request.view request) $ \shown -> do
            now <- IORef.readIORef seatRef
            let seen = shown {PlayerView.priority = Nothing}
                facingNow = if Web.Question.handsOver (yours now) (facing now) seat then facing now else Just seat
            shownClocks <- facesOf now
            Monad.when (watched now /= Just seen) . Monad.forM_ (Web.Question.watching facingNow [] False request (watchedScreen lookUp seat now shownClocks shown)) $ \board -> do
              IORef.writeIORef seatRef now {watched = Just seen}
              Browser.display page (fmap Screen.MkQuestion board)
          pure (Just reply)
        conceded = if Set.member seat (conceding state) && kind == Text.pack "Concede" then Just (R.Text (Text.pack "Concedes")) else Nothing
    case (Monad.mplus conceded (Web.Question.automatic (level state) request), auto, skipped) of
      (Just reply, _, _) -> watch reply
      (_, Just reply, _) -> watch reply
      (_, _, Just reply) -> watch reply
      _ -> do
        Monad.when (Web.Question.handsOver (yours state) (facing state) seat) $ do
          Browser.display page . fmap Screen.MkQuestion $ Web.Question.handOff request
          untilReady
        IORef.modifyIORef' seatRef $ \s -> s {facing = Just seat}
        -- A tap made while the board was only being watched is dropped,
        -- but for the auto-pass level.
        keepLevel
        -- The question replaces the watched board, which is drawn afresh after.
        IORef.modifyIORef' seatRef $ \st -> st {watched = Nothing}
        reply <- answer request (Web.Question.initial request)
        -- Only what the seat decided itself, so the log's new entries and
        -- a turn begun since (Web.Question.turnBegun) outlast a pass made
        -- for it.
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
    answer pending picked = asking pending picked Nothing Nothing Text.empty
    asking pending picked reading focus typed = do
      state <- IORef.readIORef seatRef
      shownClocks <- facesOf state
      let asked = Question.seat (Request.question pending)
          screen = Web.Question.MkScreen picked reading (loggedFor asked state) (freshOf asked state) focus (level state) (foldMap (Web.Card.written lookUp) (Request.view pending)) (naming asked state) shownClocks typed
      Browser.display page . fmap Screen.MkQuestion $ Web.Question.view pending screen
      (event, value) <- Browser.next page
      case event of
        Screen.MkQuestion (Web.Question.MkRead object)
          | Just r <- readingOf lookUp pending object True -> asking pending picked (Just r) focus typed
        -- A preview shows only while no tapped sheet is open, and ends only
        -- itself, never the tapped sheet (the pointer leaving the tile for
        -- that sheet's scrim ends a preview).
        Screen.MkQuestion (Web.Question.MkPeek object)
          | maybe True (not . Web.Card.pinned) reading,
            Just r <- readingOf lookUp pending object False ->
              asking pending picked (Just r) focus typed
          | otherwise -> asking pending picked reading focus typed
        Screen.MkQuestion Web.Question.MkUnpeek
          | maybe False (not . Web.Card.pinned) reading -> asking pending picked Nothing focus typed
          | otherwise -> asking pending picked reading focus typed
        -- The home screen runs here, the question waiting; giving the game
        -- up unwinds it (game).
        Screen.MkQuestion Web.Question.MkHome -> do
          left <- leave (Request.view pending)
          case left of
            MkResume -> asking pending picked reading focus typed
            MkAbandon -> Exception.throwIO MkAbandonedGame
        Screen.MkQuestion e -> case Web.Question.step pending picked e value of
          Web.Question.MkReplied answered -> do
            Monad.when (e == Web.Question.MkAuto) . IORef.modifyIORef' seatRef $ \s -> s {paying = Map.insert asked pending (paying s)}
            case e of
              Web.Question.MkSkip stop -> IORef.modifyIORef' seatRef $ \s -> s {skipping = Map.insert asked stop (skipping s)}
              _ -> pure ()
            Monad.when (e == Web.Question.MkConcede) . IORef.modifyIORef' seatRef $ \s -> s {conceding = Set.insert asked (conceding s)}
            pure (Just answered)
          Web.Question.MkConceded -> do
            left <- concedeAtOnce seatRef concede asked
            -- Not left at once: passes, conceding at its next concession
            -- question.
            pure (if left then Nothing else fmap R.Number (Web.Question.passIndex pending))
          -- A pick made from the card's sheet leaves the sheet open.
          Web.Question.MkPicking now -> do
            case e of
              Web.Question.MkLevel l -> IORef.modifyIORef' seatRef $ \st -> st {level = l}
              _ -> pure ()
            asking pending now (if isToggle e then reading else Nothing) (focusAfter e focus) (if e == Web.Question.MkFilter then value else typed)
          Web.Question.MkRefused problem -> asking pending {Request.problem = Just problem} picked Nothing focus typed
        _ -> asking pending picked reading focus typed
    focusAfter e focus = case e of
      Web.Question.MkFocus g -> if focus == Just g then Nothing else Just g
      Web.Question.MkClear -> Nothing
      _ -> focus
    isToggle e = case e of
      Web.Question.MkToggle _ _ -> True
      Web.Question.MkFocus _ -> True
      _ -> False

-- | Seat `seat` conceding at once where it can (Web.Question.MkConceded),
-- kept among those that conceded, then given to `concede`.
concedeAtOnce :: IORef.IORef Seat -> (PlayerId.PlayerId -> IO a) -> PlayerId.PlayerId -> IO a
concedeAtOnce seatRef concede seat = do
  IORef.modifyIORef' seatRef $ \s -> s {conceding = Set.insert seat (conceding s)}
  concede seat

-- | Card `object`'s text as the request's board shows it, `pinned` if
-- tapped rather than previewed.
readingOf :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Request.Request -> ObjectId.ObjectId -> Bool -> Maybe Web.Card.Reading
readingOf lookUp request object pinned = do
  shown <- Request.view request
  o <- Wording.recalled shown object
  pure (Web.Card.MkReading object (either (pure . Text.pack . Exception.displayException) id (Web.Card.oracle lookUp o)) pinned)

-- | The screen a seat watches the board `shown` with: its log, the level
-- it plays at, and these clocks.
watchedScreen :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> PlayerId.PlayerId -> Seat -> [Web.Clock.Face] -> PlayerView.PlayerView -> Web.Question.Screen
watchedScreen lookUp seat state shownClocks shown = Web.Question.blank {Web.Question.logged = loggedFor seat state, Web.Question.fresh = freshOf seat state, Web.Question.level = level state, Web.Question.written = Web.Card.written lookUp shown, Web.Question.naming = naming seat state, Web.Question.clocks = shownClocks}

-- | Every timed seat's clock as it reads now.
facesOf :: Seat -> IO [Web.Clock.Face]
facesOf state = do
  at <- Clock.getMonotonicTime
  (epoch, _) <- Browser.clock
  pure (Web.Clock.faces at epoch (clocks state))

-- | Whether the screen names the seat it shows (Web.Question.naming): the
-- person at it plays that seat and another, or it is handed between people.
naming :: PlayerId.PlayerId -> Seat -> Bool
naming seat state = handed state || (Set.size (yours state) > 1 && Set.member seat (yours state))

-- | Runs `decide`, waiting on another browser's seat, the page meanwhile
-- live (watcher), a concession given to `concede`.
awaiting :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page Screen.Event -> IORef.IORef Seat -> (Maybe PlayerView.PlayerView -> IO Leave) -> (PlayerId.PlayerId -> IO ()) -> (Seat -> [PlayerId.PlayerId]) -> IO a -> IO a
awaiting lookUp page seatRef leave concede deciders decide = do
  readingRef <- IORef.newIORef Nothing
  let (draw, tap) = watcher lookUp page seatRef leave concede readingRef deciders
  draw
  Browser.meanwhile page tap decide

-- | The page while the game waits on others, drawn and taking a tap: the
-- board the seat the screen faces sees (current), the seats `deciders`
-- names as those the game waits on; a card tapped there shows its text
-- through `lookUp`, kept in `readingRef`, an auto-pass level tapped is kept,
-- the menu leaves for the home screen through `leave`, and a concession is
-- kept for that seat's next concession question (exchange) and given to
-- `concede` at once.
watcher :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page Screen.Event -> IORef.IORef Seat -> (Maybe PlayerView.PlayerView -> IO Leave) -> (PlayerId.PlayerId -> IO ()) -> IORef.IORef (Maybe Web.Card.Reading) -> (Seat -> [PlayerId.PlayerId]) -> (IO (), (Screen.Event, Text.Text) -> IO ())
watcher lookUp page seatRef leave concede readingRef deciders = (draw, tap)
  where
    draw = do
      state <- IORef.readIORef seatRef
      reading <- IORef.readIORef readingRef
      shownClocks <- facesOf state
      -- The board watched next is drawn afresh, over this one; its log
      -- is read.
      IORef.writeIORef seatRef (maybe id witness (current state >>= Request.view) state) {watched = Nothing}
      let board = do
            request <- current state
            shown <- Request.view request
            Web.Question.watching (facing state) (deciders state) (any (`Set.member` conceding state) (facing state)) request (watchedScreen lookUp (Question.seat (Request.question request)) state shownClocks shown) {Web.Question.reading = reading}
      Browser.display page (maybe (message [Text.pack (Web.Question.waitingOn (deciders state))]) (fmap Screen.MkQuestion) board)
    -- Every tap redraws the page, which a tap shows busy until then.
    tap (event, _) = do
      state <- IORef.readIORef seatRef
      case event of
        Screen.MkQuestion (Web.Question.MkRead object) -> IORef.writeIORef readingRef (current state >>= \request -> readingOf lookUp request object True)
        Screen.MkQuestion Web.Question.MkClose -> IORef.writeIORef readingRef Nothing
        Screen.MkQuestion (Web.Question.MkLevel l) -> IORef.writeIORef seatRef state {level = l}
        Screen.MkQuestion Web.Question.MkConcede -> Monad.forM_ (facing state) (concedeAtOnce seatRef concede)
        -- The home screen runs here, the game's channels held for it;
        -- giving the game up unwinds it (game).
        Screen.MkQuestion Web.Question.MkHome -> do
          left <- leave (current state >>= Request.view)
          Monad.when (left == MkAbandon) (Exception.throwIO MkAbandonedGame)
        _ -> pure ()
      draw

-- | A search bot's seat deciding, `thinker`: since when, in seconds of the
-- monotonic clock, the page last had a turn, and whether it was drawn
-- waiting on that seat yet (breathing).
data Breath = MkBreath
  { thinker :: PlayerId.PlayerId,
    since :: Double,
    drawn :: Bool
  }

-- | While a search bot decides (Search.search's breathe), the page given a
-- turn once 'breath' has passed since it last had one, or since the bot
-- was asked (`breathRef`): the first turn draws the board waiting on the
-- bot, and each tap taken is handled as while another browser's seat
-- decides (watcher), its card read kept in `readingRef`, a concession
-- given to `concede`, and each guest heard (Browser.hearNow).
breathing :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page Screen.Event -> IORef.IORef Seat -> (Maybe PlayerView.PlayerView -> IO Leave) -> (PlayerId.PlayerId -> IO ()) -> IORef.IORef (Maybe Web.Card.Reading) -> IORef.IORef (Maybe Breath) -> IO ()
breathing lookUp page seatRef leave concede readingRef breathRef = do
  now <- Clock.getMonotonicTime
  thinking <- IORef.readIORef breathRef
  Monad.forM_ thinking $ \b -> Monad.when (now - since b >= breath) $ do
    IORef.writeIORef breathRef (Just b {since = now, drawn = True})
    let (draw, tap) = watcher lookUp page seatRef leave concede readingRef (const [thinker b])
    Browser.pause
    Browser.hearNow page
    Monad.unless (drawn b) draw
    Browser.drain page tap

-- | How long, in seconds, a search bot decides between the page's turns.
breath :: Double
breath = 0.1

-- | The page's state with the request kept as the last its seat was asked,
-- newer than the board it saw stand before.
keepAsked :: Request.Request -> Seat -> Seat
keepAsked request state =
  let seat = Question.seat (Request.question request)
   in state {latest = Map.insert seat request (latest state), standing = Map.delete seat (standing state)}

-- | The page's state with the game as `seat` sees it at a decision another
-- seat makes, at this depth (Request.depth).
stand :: PlayerId.PlayerId -> PlayerView.PlayerView -> Natural -> Seat -> Seat
stand seat shown depth state = state {standing = Map.insert seat (shown, depth) (standing state)}

-- | What the seat the screen faces is shown while another seat decides: the
-- game as it stands where it moved on since the seat was last asked, asking
-- nothing, else that request.
current :: Seat -> Maybe Request.Request
current state = do
  seat <- facing state
  case Map.lookup seat (standing state) of
    Just (shown, depth) -> Just (Request.MkRequest (Just shown) (Question.MkQuestion seat Text.empty R.Null Nothing Nothing) Nothing depth)
    Nothing -> Map.lookup seat (latest state)

-- | What the page keeps between request lines: the seat the screen last
-- showed, the seats one person plays, whether another plays here, the
-- seats that chose auto for the payment under way, what each seat has seen
-- happen, when it last answered, and the clocks.
data Seat = MkSeat
  { facing :: Maybe PlayerId.PlayerId,
    -- | The seats the person at the screen plays, never handed the screen
    -- between them (Web.Question.handsOver).
    yours :: Set.Set PlayerId.PlayerId,
    -- | Whether another person plays at this screen, handed it before each
    -- of their questions (Link.MkHandedOver).
    handed :: Bool,
    -- | The seats that chose auto for the payment under way, each with the
    -- source question auto last answered for it, whose source's mana auto
    -- then chooses (Web.Question.paying).
    paying :: Map.Map PlayerId.PlayerId Request.Request,
    -- | How much the page answers for every seat it plays (Web.Question.Level).
    level :: Web.Question.Level,
    -- | How far each seat asked to pass (Web.Question.MkSkip).
    skipping :: Map.Map PlayerId.PlayerId Web.Question.Until,
    -- | The seats that conceded (Web.Question.MkConcede).
    conceding :: Set.Set PlayerId.PlayerId,
    -- | The view last drawn while watching, less whose priority it was.
    watched :: Maybe PlayerView.PlayerView,
    -- | What its seats have seen: the seat the page plays's log is the one
    -- the end shows.
    sighted :: Ending.Seen,
    -- | How many entries each seat had logged when it last answered itself.
    decided :: Map.Map PlayerId.PlayerId Int,
    -- | The request each seat the page plays was last asked.
    latest :: Map.Map PlayerId.PlayerId Request.Request,
    -- | The game as each seat the page plays sees it at a decision another
    -- seat makes since, and its depth (stand).
    standing :: Map.Map PlayerId.PlayerId (PlayerView.PlayerView, Natural),
    -- | Every timed seat's clock (Web.Clock): a guest's page keeps what the
    -- host last sent it (Web.Clock.heard).
    clocks :: Web.Clock.Clocks
  }

-- | The page's state before its first request line, facing the seat it
-- plays and playing only that one, no one else at the screen, no seat
-- timed, at that auto-pass level, the seat having read what was `seen` already: a
-- rejoining guest's game before its page reloaded (Link.MkStart).
newSeat :: PlayerId.PlayerId -> Ending.Seen -> Web.Question.Level -> Seat
newSeat seat seen initial = MkSeat (Just seat) (Set.singleton seat) False Map.empty initial Map.empty Set.empty Nothing seen (Map.singleton seat (Seq.length (Ending.loggedFor seat seen))) Map.empty Map.empty Web.Clock.none

-- | The entries a seat has read so far, oldest first.
loggedFor :: PlayerId.PlayerId -> Seat -> Seq.Seq Log.Entry
loggedFor seat = Ending.loggedFor seat . sighted

-- | How many entries a seat has logged since it last answered itself.
freshOf :: PlayerId.PlayerId -> Seat -> Natural
freshOf seat state = Int.toNaturalSaturating (Seq.length (loggedFor seat state) - Map.findWithDefault 0 seat (decided state))

-- | The page's state with the entries of the view its seat has not read yet
-- logged for that seat.
witness :: PlayerView.PlayerView -> Seat -> Seat
witness view state = state {sighted = Ending.see view (sighted state)}

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
