-- Covers Parun.Web.Link, Parun.Play.Protocol's bySeat and Parun.Web.Lobby's
-- invites: games whose guests' seats are played from other pages, each over
-- its own channel held in memory, and the agreement that starts them.
module Parun.RemoteSeatSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.BotSpec as BotSpec
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Search as Search
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Weights as Weights
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Lobby as Lobby

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "RemoteSeat" $ do
  Spec.it s "a game with an invited player starts only once both players are ready, and a change of deck clears both" $ do
    let one = Text.pack "sets/a/one"
        two = Text.pack "sets/a/two"
        pick name = Lobby.update (Lobby.MkPick name) Text.empty
        picked = pick one (Lobby.withDecks (Seq.fromList [one, two]) Map.empty Lobby.new)
        invited = Lobby.update (Lobby.MkInviteSlot 0) Text.empty picked
        connected = Lobby.linked 0 (\invite -> invite {Lobby.connected = True}) invited
        chosen = Lobby.heard 0 (Link.MkHello two (Text.pack "20 Forest\n")) connected
        hostReady = Lobby.update Lobby.MkReady Text.empty chosen
        guestReady = Lobby.heard 0 (Link.MkReady True) chosen
        bothReady = Lobby.heard 0 (Link.MkReady True) hostReady
        changed = pick two bothReady
        guestFlag = fmap Lobby.guestReady . Map.lookup 0 . Lobby.invites
    Spec.assertEqWith s "starts" (fmap Lobby.starts [invited, connected, chosen, hostReady, guestReady, bothReady, changed]) [False, False, False, False, False, True, False]
    Spec.assertEqWith s "the guest is shown who is ready" (fmap (fmap Link.ready . Lobby.placesFor 0) [hostReady, guestReady]) [[Just True, Just False], [Just False, Just True]]
    Spec.assertEqWith s "a change of deck clears every ready flag" (Lobby.ready changed, guestFlag changed) (False, Just False)

  Spec.it s "a game with two invited players starts only once all three are ready, and each guest is shown its own seat as its own" $ do
    let one = Text.pack "sets/a/one"
        two = Text.pack "sets/a/two"
        added = Lobby.update Lobby.MkAddBot Text.empty (Lobby.update (Lobby.MkPick one) Text.empty (Lobby.withDecks (Seq.fromList [one, two]) Map.empty Lobby.new))
        invited = Lobby.update (Lobby.MkInviteSlot 1) Text.empty (Lobby.update (Lobby.MkInviteSlot 0) Text.empty added)
        joined i = Lobby.heard i (Link.MkHello two (Text.pack "20 Forest\n")) . Lobby.linked i (\invite -> invite {Lobby.connected = True})
        chosen = joined 1 (joined 0 invited)
        hostReady = Lobby.update Lobby.MkReady Text.empty chosen
        firstReady = Lobby.heard 0 (Link.MkReady True) hostReady
        allReady = Lobby.heard 1 (Link.MkReady True) firstReady
        dropped = Lobby.linked 1 (const Lobby.newInvite) allReady
        offered = foldr (\i -> Lobby.linked i (\invite -> invite {Lobby.link = Just (Text.pack ("https://example.test/#offer=" <> show i))})) invited [0, 1]
        shown = Html.toText (Lobby.view offered)
    Spec.assertBool s (all (\i -> Text.isInfixOf (Text.pack ("id=\"invite-reply-" <> show i <> "\"")) shown && Text.isInfixOf (Text.pack ("#offer=" <> show i)) shown) [0 :: Int, 1]) "each invited seat has its own link and its own reply box"
    Spec.assertEqWith s "starts" (fmap Lobby.starts [invited, chosen, hostReady, firstReady, allReady, dropped]) [False, False, False, False, True, False]
    Spec.assertEqWith s "each guest is shown itself as You and the other as a player" (fmap (fmap Link.who . (`Lobby.placesFor` allReady)) [0, 1]) [[Text.pack "Host", Text.pack "You", Text.pack "Player 3"], [Text.pack "Host", Text.pack "Player 2", Text.pack "You"]]
    Spec.assertEqWith s "a guest dropping clears every ready flag" (Lobby.ready dropped, fmap Lobby.guestReady (Lobby.invites dropped)) (False, Map.fromList [(0, False), (1, False)])

  Spec.it s "a whole game in which the guest's seat is answered only through the remote exchange, and the guest sees only its own view" $ do
    matchup <- BotSpec.bears s registry
    wholeGame s matchup [S.bob]

  Spec.it s "a whole game with two guests, each answering only its own seat over its own channel and seeing only its own view" $ do
    matchup <- BotSpec.bears s registry
    wholeGame s (matchup <> pure (S.carol, snd (NonEmpty.head matchup))) [S.bob, S.carol]

  Spec.it s "a guest whose connection drops mid-game rejoins, is sent its seat and the request again, and the game finishes" $ do
    matchup <- BotSpec.bears s registry
    (outcome, wire) <- State.runStateT (hosted [S.bob] matchup) (MkWire Map.empty Map.empty Map.empty (Map.singleton S.bob 5) 0 [])
    Spec.assertEqWith s "finished" (Monad.void outcome) (Right ())
    Spec.assertEqWith s "rejoined once" (rejoins wire) 1
    Spec.assertEqWith s "the guest is sent its seat again" (starts wire) [S.bob]

  Spec.it s "a game picked up from its seed and the replies so far asks what the game played through asked, and ends the same" $ do
    matchup <- BotSpec.bears s registry
    let replayed whole k = State.runStateT (resumedGame matchup) (MkResumed (take k whole) 0 [] [] [])
    (outcome, whole) <- replayed [] 0
    let replies = reverse (kept whole)
        k = div (length replies) 2
    (again, resumed) <- replayed replies k
    Spec.assertBool s (k > 0) "the game asks something"
    Spec.assertEqWith s "ends the same" (fmap fst again) (fmap fst outcome)
    Spec.assertEqWith s "the requests made again are seen, not asked" (replays resumed) k
    Spec.assertEqWith s "asks what the game played through asked after them" (reverse (live resumed)) (drop k (reverse (live whole)))
    Spec.assertEqWith s "keeps only the replies made after them" (take k replies <> reverse (kept resumed)) replies

  Spec.it s "a game against the search bot picked up from its seed and the replies so far, the bot's among them, searches none of the bot's decisions made before, and ends the same" $ do
    matchup <- BotSpec.bears s registry
    let replayed whole k = State.runStateT (againstSearch matchup) (MkResumed (take k whole) 0 [] [] [])
    (outcome, whole) <- replayed [] 0
    let replies = reverse (kept whole)
        k = div (length replies) 2
    (again, resumed) <- replayed replies k
    Spec.assertBool s (any (< k) (searches whole)) "the bot decides before the replies stored run out"
    Spec.assertEqWith s "ends the same" (fmap fst again) (fmap fst outcome)
    Spec.assertEqWith s "searches only the bot's decisions after them" (reverse (searches resumed)) [n - k | n <- reverse (searches whole), n >= k]
    Spec.assertEqWith s "keeps only the replies made after them, the bot's among them" (take k replies <> reverse (kept resumed)) replies

-- A game between these seats, every seat in `guests` played from its own
-- page over its own channel and the rest from the host's: it finishes, each
-- page answers only its own seat and is sent only its own seat's view, and
-- each guest is told how it ended.
wholeGame :: (Monad m) => Spec.Spec m n -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> [PlayerId.PlayerId] -> m ()
wholeGame s matchup guests = do
  (outcome, wire) <- State.runStateT (hosted guests matchup) (MkWire Map.empty Map.empty Map.empty Map.empty 0 [])
  case outcome of
    Right (Result.Won _, _) -> do
      told <- State.execStateT (Monad.forM_ guests $ \g -> Link.send (channel g) (Link.MkEnd Ending.MkStopped [Text.pack "over"] Text.empty)) wire
      Spec.assertEqWith s "each guest is told how it ended" (ended told) (Map.fromList [(g, Link.MkEnded Ending.MkStopped [Text.pack "over"] Text.empty) | g <- guests])
    _ -> Spec.assertBool s False ("finished: " <> show (fmap fst outcome))
  let host = [pid | (pid, _) <- NonEmpty.toList matchup, pid `notElem` guests]
      askedOf which = Map.findWithDefault [] which (asked wire)
  Spec.assertBool s (not (null (askedOf hostPage)) && all ((`elem` host) . fst) (askedOf hostPage)) "the host's page answers only its own seat"
  Monad.forM_ guests $ \g -> do
    Spec.assertBool s (not (null (askedOf (Just g))) && all ((== g) . fst) (askedOf (Just g))) ("guest " <> show g <> "'s page answers only its own seat")
    Spec.assertEqWith s ("guest " <> show g <> " is sent only its own seat's view") (ListUtils.nubOrd (Maybe.mapMaybe snd (askedOf (Just g)))) [g]

-- Every page and the channels between them: the lines the host has yet to
-- read from each guest, the seat of each request each page (Nothing for the
-- host's, a guest's seat for its own) answered and whose view it carried,
-- and the end each guest was told; how many more lines each guest's channel
-- carries before it drops once, how many times a guest rejoined, and each
-- seat a guest was sent again.
data Wire = MkWire
  { inbox :: Map.Map PlayerId.PlayerId [Text.Text],
    asked :: Map.Map (Maybe PlayerId.PlayerId) [(PlayerId.PlayerId, Maybe PlayerId.PlayerId)],
    ended :: Map.Map PlayerId.PlayerId Link.Heard,
    dropping :: Map.Map PlayerId.PlayerId Int,
    rejoins :: Int,
    starts :: [PlayerId.PlayerId]
  }

build :: Text.Text
build = Text.pack "test"

-- The host's game: every seat over the line protocol, each guest's requests
-- sent over its channel and every other seat's to the host's page. A guest
-- whose channel drops rejoins the first time, and not again.
hosted :: (Monad m) => [PlayerId.PlayerId] -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> State.StateT Wire m (Either Text.Text (Result.Result, GameState.GameState))
hosted guests matchup = Game.run (S.fixtureRegistry []) Nothing Limits.standard [] (Map.fromList [(pid, Game.wired (S.fixtureRegistry []) (Protocol.bySeat (Map.fromList [(g, Link.remote rejoined (channel g)) | g <- guests]) (page hostPage))) | (pid, _) <- NonEmpty.toList matchup]) (S.seeded 1) Map.empty matchup

-- A guest rejoining, which connects the first time and not again.
rejoined :: (Monad m) => State.StateT Wire m Bool
rejoined = do
  State.modify' (\w -> w {rejoins = rejoins w + 1})
  (<= 1) . rejoins <$> State.get

-- A game the host's page plays every seat of, picked up after the replies
-- stored (Protocol.resumed): the requests seen while they last, and each
-- one asked and reply kept after; and before each search the bot made, how
-- many replies were kept.
data Resumed = MkResumed
  { stored :: [R.Reply],
    replays :: Int,
    live :: [Request.Request],
    kept :: [R.Reply],
    searches :: [Int]
  }

resumedGame :: (Monad m) => NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> State.StateT Resumed m (Either Text.Text (Result.Result, GameState.GameState))
resumedGame matchup = Game.run (S.fixtureRegistry []) Nothing Limits.standard [] (Map.fromList [(pid, Game.wired (S.fixtureRegistry []) resuming) | (pid, _) <- NonEmpty.toList matchup]) (S.seeded 1) Map.empty matchup

-- resumedGame with Bob's seat the search bot ('searching'), its answers
-- stored and kept alongside the page's (Game.remembered).
againstSearch :: (Monad m) => NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> State.StateT Resumed m (Either Text.Text (Result.Result, GameState.GameState))
againstSearch matchup =
  let seat pid = if pid == S.bob then Game.remembered nextStored (\reply -> State.modify' (\r -> r {kept = reply : kept r})) searching else Game.wired (S.fixtureRegistry []) resuming
   in Game.run (S.fixtureRegistry []) Nothing Limits.standard [] (Map.fromList [(pid, seat pid) | (pid, _) <- NonEmpty.toList matchup]) (S.seeded 1) Map.empty matchup

-- The search bot, at 2 samples cut off after 40 prompts, noting each search.
searching :: (Monad m) => Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT Resumed m))
searching = Answerer.MkAnswerer $ \prompted -> do
  Monad.when (Maybe.isJust (Protocol.requestFor prompted)) $ Game.underneath (State.modify' (\r -> r {searches = length (kept r) : searches r}))
  Answerer.unwrap (Search.search (S.fixtureRegistry []) budget Weights.neutral Search.known) prompted
  where
    budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.playout = Playout.Random}

-- Each request answered from the replies stored, then by a page that passes.
resuming :: (Monad m) => Protocol.Exchange (State.StateT Resumed m)
resuming = Protocol.resumed nextStored (const (State.modify' (\r -> r {replays = replays r + 1}))) (\reply -> State.modify' (\r -> r {kept = reply : kept r})) asking
  where
    asking request = State.modify' (\r -> r {live = request : live r}) >> pure (Just (Right (answer request)))

-- The host's page, as `asked` keys it.
hostPage :: Maybe PlayerId.PlayerId
hostPage = Nothing

-- The host's end of this guest's channel: each message rendered and read
-- back as a line, answered at once by the guest's page (Link.guest), whose
-- reply waits for the host to read.
channel :: (Monad m) => PlayerId.PlayerId -> Link.Channel (State.StateT Wire m)
channel g = Link.MkChannel send (received g)
  where
    send message = case Link.decode build (Link.encode build message) of
      Left _ -> pure ()
      Right (Link.MkStart seat) -> State.modify' (\w -> w {starts = starts w <> [seat]})
      Right line -> do
        heard <- Link.guest (page (Just g)) line
        case heard of
          Link.MkReply reply -> State.modify' (\w -> w {inbox = Map.insertWith (flip (<>)) g [Link.encode build reply] (inbox w)})
          Link.MkEnded {} -> State.modify' (\w -> w {ended = Map.insert g heard (ended w)})
          Link.MkIgnored -> pure ()

-- The next line this guest's page sent, read; Nothing once there is none,
-- and once the channel drops, which loses the line unread.
received :: (Monad m) => PlayerId.PlayerId -> State.StateT Wire m (Maybe Link.Message)
received g = do
  w <- State.get
  case (Map.findWithDefault [] g (inbox w), Map.lookup g (dropping w)) of
    (_ : _, Just 0) -> State.put w {inbox = Map.delete g (inbox w), dropping = Map.delete g (dropping w)} >> pure Nothing
    (line : rest, left) -> State.put w {inbox = Map.insert g rest (inbox w), dropping = maybe (dropping w) (\n -> Map.insert g (n - 1) (dropping w)) left} >> pure (either (const Nothing) Just (Link.decode build line))
    ([], _) -> pure Nothing

-- A page's own exchange, each request it answers recorded under it.
page :: (Monad m) => Maybe PlayerId.PlayerId -> Protocol.Exchange (State.StateT Wire m)
page which request = State.modify' (\w -> w {asked = Map.insertWith (flip (<>)) which [seen request] (asked w)}) >> pure (Just (Right (answer request)))

seen :: Request.Request -> (PlayerId.PlayerId, Maybe PlayerId.PlayerId)
seen request = (Question.seat (Request.question request), fmap PlayerView.viewer (Request.view request))

-- Passes, keeps, declines and discards the first cards offered, as a page
-- whose player does nothing else would.
answer :: Request.Request -> R.Reply
answer request =
  let question = Request.question request
      offer = Question.offer question
      field key = case offer of
        R.Object pairs -> lookup (Text.pack key) pairs
        _ -> Nothing
   in case Text.unpack (Question.kind question) of
        "Concede" -> R.Text (Text.pack "Continues")
        "DeclareMulligan" -> R.Text (Text.pack "Keep")
        "ChooseAction" -> case offer of
          R.Array actions -> maybe R.Null (R.Number . toInteger) (List.elemIndex (R.Object [(Text.pack "action", R.Text (Text.pack "Pass"))]) actions)
          _ -> R.Null
        "ChooseDiscard" -> case (field "from", field "count") of
          (Just (R.Array from), Just (R.Number count)) -> R.Array (List.genericTake count from)
          _ -> R.Null
        _ -> R.Null

-- The next reply stored, taken.
nextStored :: (Monad m) => State.StateT Resumed m (Maybe R.Reply)
nextStored = State.state $ \r -> case stored r of
  reply : rest -> (Just reply, r {stored = rest})
  [] -> (Nothing, r)
