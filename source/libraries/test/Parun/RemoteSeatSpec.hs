-- Covers Parun.Web.Link, Parun.Play.Protocol's bySeat and Parun.Web.Lobby's
-- invites and players here: games whose guests' seats are played from other
-- pages, each over its own channel held in memory, the agreement that starts
-- them, and a game whose seats share the host's page.
module Parun.RemoteSeatSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.BotSpec as BotSpec
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Search as Search
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.SearchMethod as SearchMethod
import qualified Parun.Types.Weights as Weights
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Question as Question.Web

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "RemoteSeat" $ do
  Spec.it s "a game with an invited player starts only once both players are ready, and a change of deck clears both" $ do
    let one = Text.pack "sets/a/one"
        two = Text.pack "sets/a/two"
        pick name = Lobby.update (Lobby.MkPick name) Text.empty
        picked = pick one (Lobby.withDecks (Seq.fromList [one, two]) Map.empty Map.empty Lobby.new)
        invited = Lobby.update (Lobby.MkAdd Lobby.MkInvited) Text.empty picked
        connected = Lobby.linked 0 (\invite -> invite {Lobby.connected = True}) invited
        chosen = Lobby.heard 0 (Link.MkHello two (Text.pack "20 Forest\n")) connected
        hostReady = Lobby.update Lobby.MkReady Text.empty chosen
        guestReady = Lobby.heard 0 (Link.MkReady True) chosen
        bothReady = Lobby.heard 0 (Link.MkReady True) hostReady
        changed = pick two bothReady
        guestFlag = fmap Lobby.guestReady . Map.lookup 0 . Lobby.invites
    Spec.assertEqWith s "starts" (fmap Lobby.starts [invited, connected, chosen, hostReady, guestReady, bothReady, changed]) [False, False, False, False, False, True, False]
    Spec.assertEqWith s "the guest is shown who is ready" (fmap (fmap Link.ready . Lobby.placesFor uncoloured uncoloured 0) [hostReady, guestReady]) [[Just True, Just False], [Just False, Just True]]
    Spec.assertEqWith s "a change of deck clears every ready flag" (Lobby.ready changed, guestFlag changed) (False, Just False)

  Spec.it s "a game with two invited players starts only once all three are ready, and each guest is shown its own seat as its own" $ do
    let one = Text.pack "sets/a/one"
        two = Text.pack "sets/a/two"
        picked = Lobby.update (Lobby.MkPick one) Text.empty (Lobby.withDecks (Seq.fromList [one, two]) Map.empty Map.empty Lobby.new)
        invited = Lobby.update (Lobby.MkAdd Lobby.MkInvited) Text.empty (Lobby.update (Lobby.MkAdd Lobby.MkInvited) Text.empty picked)
        joined i = Lobby.heard i (Link.MkHello two (Text.pack "20 Forest\n")) . Lobby.linked i (\invite -> invite {Lobby.connected = True})
        chosen = joined 1 (joined 0 invited)
        hostReady = Lobby.update Lobby.MkReady Text.empty chosen
        firstReady = Lobby.heard 0 (Link.MkReady True) hostReady
        allReady = Lobby.heard 1 (Link.MkReady True) firstReady
        dropped = Lobby.linked 1 (const (Lobby.newInvite 1)) allReady
        offered = foldr (\i -> Lobby.linked i (\invite -> invite {Lobby.link = Just (Text.pack ("https://example.test/#offer=" <> show i))})) invited [0, 1]
        shown = Html.toText (Lobby.view offered)
    Spec.assertBool s (all (\i -> Text.isInfixOf (Text.pack ("id=\"invite-reply-" <> show i <> "\"")) shown && Text.isInfixOf (Text.pack ("#offer=" <> show i)) shown) [0 :: Int, 1]) "each invited seat has its own link and its own reply box"
    Spec.assertEqWith s "starts" (fmap Lobby.starts [invited, chosen, hostReady, firstReady, allReady, dropped]) [False, False, False, False, True, False]
    Spec.assertEqWith s "each guest is shown itself as You and the other as a player" (fmap (fmap Link.who . (\i -> Lobby.placesFor uncoloured uncoloured i allReady)) [0, 1]) [[Text.pack "Player 1", Text.pack "You", Text.pack "Player 3"], [Text.pack "Player 1", Text.pack "Player 2", Text.pack "You"]]
    Spec.assertEqWith s "a guest dropping clears every ready flag" (Lobby.ready dropped, fmap Lobby.guestReady (Lobby.invites dropped)) (False, Map.fromList [(0, False), (1, False)])

  Spec.it s "a whole game in which the guest's seat is answered only through the remote exchange, and the guest sees only its own view" $ do
    matchup <- BotSpec.bears s registry
    wholeGame s matchup [S.bob]

  Spec.it s "a whole game with two guests, each answering only its own seat over its own channel, seeing only its own view and told which other seat decides" $ do
    matchup <- BotSpec.bears s registry
    wholeGame s (matchup <> pure (S.carol, snd (NonEmpty.head matchup))) [S.bob, S.carol]

  -- A seat deciding again on a board that moved on, which the whole games
  -- above never reach, every seat always passing (#9704).
  Spec.it s "a guest is told of a seat deciding again once the board its seat sees changes, and not while it stays the same" $ do
    let hurt = S.threePlayerGame {GameState.players = Map.adjust (\p -> p {Player.life = 17}) S.alice (GameState.players S.threePlayerGame)}
        channels :: Map.Map PlayerId.PlayerId (Link.Channel (State.State [(PlayerId.PlayerId, Link.Message)]))
        channels = Map.fromList [(g, Link.MkChannel (\message -> State.modify' (<> [(g, message)])) (pure Nothing)) | g <- [S.bob, S.carol]]
        decisions = [(S.alice, S.threePlayerGame), (S.alice, S.threePlayerGame), (S.alice, hurt), (S.bob, hurt)]
        sent = State.execState (Monad.foldM (\last' (seat, gs) -> Link.telling channels seat gs 0 last') Map.empty decisions) []
        told g seat gs = (g, Link.MkDeciding seat ((PlayerView.Engine.viewFor g gs) {PlayerView.priority = Nothing}) 0)
    Spec.assertEq s sent [told S.bob S.alice S.threePlayerGame, told S.carol S.alice S.threePlayerGame, told S.bob S.alice hurt, told S.carol S.alice hurt, told S.carol S.bob hurt]

  Spec.it s "a whole game with a player on the host's screen: the lobby's options seat no bot there, and the screen is handed between the seats" $ do
    (outcome, inOrder, options) <- onOneScreen s registry
    let seats = fmap (Question.seat . Request.question) inOrder
        handed = fmap (Html.content . Question.Web.handOff) (handedOver (Set.singleton S.alice) inOrder)
    Spec.assertBool s (case outcome of Right (Result.Won _, _) -> True; _ -> False) ("finished: " <> show (fmap fst outcome))
    Spec.assertEqWith s "both seats asked on the one page" (ListUtils.nubOrd seats) [S.alice, S.bob]
    Spec.assertBool s (any (Text.isPrefixOf (Text.pack "Pass the screen to Player 2 to ")) handed && any (Text.isPrefixOf (Text.pack "Pass the screen to Player 1 to ")) handed) "handed each way, saying what is waiting"
    Spec.assertEqWith s "no bot seated" (PlayOptions.bots options) Map.empty

  Spec.it s "a whole goldfish game, both seats played by the one person at the screen: never handed over, each question naming its seat" $ do
    (outcome, inOrder, _) <- onOneScreen s registry
    let seats = fmap (Question.seat . Request.question) inOrder
        named seat = [Html.content (Question.Web.view request Question.Web.blank {Question.Web.naming = True}) | request <- inOrder, Question.seat (Request.question request) == seat]
    Spec.assertBool s (case outcome of Right (Result.Won _, _) -> True; _ -> False) ("finished: " <> show (fmap fst outcome))
    Spec.assertEqWith s "both seats asked on the one page" (ListUtils.nubOrd seats) [S.alice, S.bob]
    Spec.assertEqWith s "no hand-off" (length (handedOver (Set.fromList [S.alice, S.bob]) inOrder)) 0
    Spec.assertBool s (all (Text.isInfixOf (Text.pack "You're playing Player 2.")) (named S.bob) && all (Text.isInfixOf (Text.pack "You're playing Player 1.")) (named S.alice)) "each question names its seat"

  Spec.it s "a guest whose connection drops mid-game rejoins, is sent the match-up, its seat and the request again, and the game finishes" $ do
    matchup <- BotSpec.bears s registry
    (outcome, wire) <- State.runStateT (hosted [S.bob] matchup) (MkWire Map.empty Map.empty Map.empty (Map.singleton S.bob 40) 0 [] Map.empty Map.empty [] Map.empty Map.empty Map.empty)
    Spec.assertEqWith s "finished" (Monad.void outcome) (Right ())
    Spec.assertEqWith s "rejoined once" (rejoins wire) 1
    let firstTurn = fmap fst . Maybe.listToMaybe
    -- The guest's page reloaded as it rejoined and starts its log from what
    -- its seat was sent (#9669): the whole game, each entry once.
    let own = Map.findWithDefault Ending.unseen S.bob (pageSeen wire)
    Spec.assertEqWith s "the rejoined guest's log opens before the first turn" (Seq.lookup 0 (Ending.loggedFor S.bob own)) (Just Log.MkPregame)
    Spec.assertEqWith s "the rejoined guest's log is everything its seat was sent" (Ending.loggedFor S.bob own) (Ending.loggedFor S.bob (Map.findWithDefault Ending.unseen S.bob (hostSeen wire)))
    case sentAgain wire of
      [Link.MkMatchup again, Link.MkStart seat before] -> do
        Spec.assertEqWith s "the guest is sent the match-up, then its seat, again" (again, seat) (played, S.bob)
        Spec.assertBool s (fmap fst (Seq.lookup (Seq.length (Ending.sampledLives before) - 1) (Ending.sampledLives before)) > Just 1) "setup: the guest rejoins past turn 1"
      other -> Spec.assertBool s False ("the guest is sent again: " <> show other)
    -- The host's ending holds the game from its start (#9663).
    told <- State.execStateT (Link.send (channel S.bob) (Link.MkEnd (Link.ending Ending.MkLost [] Text.empty S.bob False outcome (Map.findWithDefault Ending.unseen S.bob (hostSeen wire))))) wire
    case Map.lookup S.bob (ended told) of
      Just (Link.MkEnded ending) -> do
        Spec.assertEqWith s "the guest's life chart starts on turn 1" (firstTurn (Ending.trail ending)) (Just 1)
        Spec.assertEqWith s "the guest's log opens before the first turn" (Seq.lookup 0 (Ending.witnessed ending)) (Just Log.MkPregame)
      other -> Spec.assertBool s False ("the guest is told no ending: " <> show other)

  Spec.it s "the match-up a game's guests are told again names every seat and every bot's deck drawn, each deck coloured" $ do
    let seat name = (Text.pack name, Text.pack "20 Forest\n")
        seats = seat "sets/a/one" NonEmpty.:| [(Text.pack "sets/a/two", Text.pack "Name: Kamahl's Temper\n20 Forest\n"), seat "sets/a/three"]
        rematched = Lobby.rematch seats (Map.singleton 1 1) Map.empty
        -- Decks this browser holds are coloured by name, a guest's by its list.
        named name = Map.findWithDefault Set.empty name (Map.fromList [(Text.pack "sets/a/one", Set.singleton Color.White), (Text.pack "sets/a/two", Set.singleton Color.Blue), (Text.pack "sets/a/three", Set.singleton Color.Black)])
        listed contents = if contents == Text.pack "20 Forest\n" then Set.singleton Color.Green else Set.empty
        told = Lobby.placesFor named listed 1 rematched
    Spec.assertEqWith s "every seat named" (fmap (\p -> (Link.who p, Link.bot p)) told) [(Text.pack "Player 1", False), (Text.pack "Player 2", True), (Text.pack "You", False)]
    Spec.assertEqWith s "every deck named, the bot's among them" (fmap Link.deck told) [Text.pack "One (A)", Text.pack "Kamahl's Temper (A)", Text.pack "Three (A)"]
    Spec.assertEqWith s "every deck coloured, the guest's by its list" (fmap Link.colours told) (fmap Set.singleton [Color.White, Color.Blue, Color.Green])

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
-- page answers only its own seat and is sent only its own seat's view, each
-- guest is told of every other seat as the seat asked changes to it, and
-- each guest is told how it ended.
wholeGame :: (Monad m) => Spec.Spec m n -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> [PlayerId.PlayerId] -> m ()
wholeGame s matchup guests = do
  (outcome, wire) <- State.runStateT (hosted guests matchup) (MkWire Map.empty Map.empty Map.empty Map.empty 0 [] Map.empty Map.empty [] Map.empty Map.empty Map.empty)
  case outcome of
    Right (Result.Won _, _) -> do
      let endingOf g = Link.ending Ending.MkStopped [Text.pack "over"] Text.empty g False outcome (Map.findWithDefault Ending.unseen g (hostSeen wire))
      told <- State.execStateT (Monad.forM_ guests $ \g -> Link.send (channel g) (Link.MkEnd (endingOf g))) wire
      Spec.assertEqWith s "each guest is told how it ended" (ended told) (Map.fromList [(g, Link.MkEnded (endingOf g)) | g <- guests])
      -- The game ends on something no request follows: a guest's ending
      -- shows it only as its end's view does (#9658).
      Monad.forM_ guests $ \g -> do
        let own = Ending.loggedFor g (Map.findWithDefault Ending.unseen g (pageSeen wire))
            (before, after) = Seq.splitAt (Seq.length own) (Ending.witnessed (endingOf g))
        Spec.assertEqWith s ("guest " <> show g <> " is told the log its page kept") before own
        Spec.assertBool s (any (\entry -> case entry of Log.MkEvent _ _ -> True; Log.MkMana _ _ -> True; Log.MkTurn {} -> False; Log.MkPregame -> False) after) ("guest " <> show g <> " is told what happened after its last request: " <> show after)
    _ -> Spec.assertBool s False ("finished: " <> show (fmap fst outcome))
  let host = [pid | (pid, _) <- NonEmpty.toList matchup, pid `notElem` guests]
      askedOf which = Map.findWithDefault [] which (asked wire)
  Spec.assertBool s (not (null (askedOf hostPage)) && all ((`elem` host) . fst) (askedOf hostPage)) "the host's page answers only its own seat"
  Monad.forM_ guests $ \g -> do
    Spec.assertBool s (not (null (askedOf (Just g))) && all ((== g) . fst) (askedOf (Just g))) ("guest " <> show g <> "'s page answers only its own seat")
    Spec.assertEqWith s ("guest " <> show g <> " is sent only its own seat's view") (ListUtils.nubOrd (Maybe.mapMaybe snd (askedOf (Just g)))) [g]
    let collapsed :: [PlayerId.PlayerId] -> [PlayerId.PlayerId]
        collapsed = fmap NonEmpty.head . NonEmpty.group
        changes = collapsed (reverse (deciders wire))
        heard = reverse (Map.findWithDefault [] g (waitedOn wire))
        toldOf = [seat | (seat, _, _) <- heard]
    Spec.assertBool s (not (null heard)) ("guest " <> show g <> " is told whom the game waits on")
    Spec.assertEqWith s ("guest " <> show g <> " is told of each other seat as the seat asked changes to it") (collapsed toldOf) (collapsed (filter (/= g) changes))
    Spec.assertEqWith s ("guest " <> show g <> " is told of every other seat") (Set.fromList toldOf) (Set.fromList [pid | (pid, _) <- NonEmpty.toList matchup, pid /= g])
    -- Not the board of the last request it was asked, which the game has
    -- moved on from (#9704).
    Spec.assertEqWith s ("guest " <> show g <> " is told the game as its seat sees it at each decision it was not shown") heard (reverse (Maybe.catMaybes (Map.findWithDefault [] g (owed wire))))

-- A whole game with both seats answered on one page, every request in
-- the order asked, and the lobby's options for it.
onOneScreen :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Either Text.Text (Result.Result, GameState.GameState), [Request.Request], PlayOptions.PlayOptions)
onOneScreen s registry = do
  matchup <- BotSpec.bears s registry
  let options = Lobby.playOptions (Map.singleton 0 (Difficulty.MkEasy, Persona.MkBalanced)) 2 S.alice (Set.singleton S.bob)
      onScreen request = State.modify' (request :) >> pure (Just (Right (answer request)))
  (outcome, requests) <- State.runStateT (Game.run (S.fixtureRegistry []) GameSettings.plain Limits.standard [] (Map.fromList [(Command.idOf n, Command.answerer (S.fixtureRegistry []) onScreen (pure Nothing) (const (pure ())) (pure Map.empty) Search.unkept (pure ()) Search.unhurried options Search.known n) | n <- [1, 2]]) (S.seeded 1) Map.empty matchup) []
  pure (outcome, reverse requests, options)

-- The requests of these, in order, before which the screen is handed
-- over, the person at it playing `yours` (Question.Web.handsOver).
handedOver :: Set.Set PlayerId.PlayerId -> [Request.Request] -> [Request.Request]
handedOver yours requests = [request | (before, request) <- zip (fmap (Question.seat . Request.question) requests) (drop 1 requests), Question.Web.handsOver yours (Just before) (Question.seat (Request.question request))]

-- Every page and the channels between them: the lines the host has yet to
-- read from each guest, the seat of each request each page (Nothing for the
-- host's, a guest's seat for its own) answered and whose view it carried,
-- and the end each guest was told; how many more lines each guest's channel
-- carries before it drops once, how many times a guest rejoined, and each
-- match-up and seat a guest was sent again; the seat each guest was last
-- told decides (Link.telling), everything each was told, latest first, and
-- every prompt's seat, latest first; each decision's seat and board as each
-- guest's seat sees it, Nothing at its own, latest first, a repeat dropped; and what each seat's page saw, a
-- guest's from what it was sent as it last rejoined, as a page reloaded
-- would (Link.MkStart), and what the host saw it sent each guest
-- (Link.seeing).
data Wire = MkWire
  { inbox :: Map.Map PlayerId.PlayerId [Text.Text],
    asked :: Map.Map (Maybe PlayerId.PlayerId) [(PlayerId.PlayerId, Maybe PlayerId.PlayerId)],
    ended :: Map.Map PlayerId.PlayerId Link.Heard,
    dropping :: Map.Map PlayerId.PlayerId Int,
    rejoins :: Int,
    sentAgain :: [Link.Message],
    lastTold :: Map.Map PlayerId.PlayerId Link.Told,
    waitedOn :: Map.Map PlayerId.PlayerId [Link.Told],
    deciders :: [PlayerId.PlayerId],
    owed :: Map.Map PlayerId.PlayerId [Maybe Link.Told],
    pageSeen :: Map.Map PlayerId.PlayerId Ending.Seen,
    hostSeen :: Map.Map PlayerId.PlayerId Ending.Seen
  }

build :: Text.Text
build = Text.pack "test"

-- The host's game: every seat over the line protocol, each guest's requests
-- sent over its channel and every other seat's to the host's page, each
-- guest told whose decision the game waits on. A guest whose channel drops
-- rejoins the first time, and not again.
hosted :: (Monad m) => [PlayerId.PlayerId] -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> State.StateT Wire m (Either Text.Text (Result.Result, GameState.GameState))
hosted guests matchup = Game.run (S.fixtureRegistry []) GameSettings.plain Limits.standard [Link.deciding decides] (Map.fromList [(pid, Game.wired (S.fixtureRegistry []) (Protocol.bySeat (Map.fromList [(g, Link.seeing (\shown -> State.modify' (\w -> w {hostSeen = seenBy shown (hostSeen w)})) (Link.remote (rejoined g) played (Map.findWithDefault Ending.unseen g . hostSeen <$> State.get) (channel g))) | g <- guests]) (page hostPage))) | (pid, _) <- NonEmpty.toList matchup]) (S.seeded 1) Map.empty matchup
  where
    decides seat gs depth = do
      let owing g = if g == seat then Nothing else Just (seat, (PlayerView.Engine.viewFor g gs) {PlayerView.priority = Nothing}, depth)
          owe g due = if take 1 due == [owing g] then due else owing g : due
      State.modify' (\w -> w {deciders = seat : deciders w, owed = foldr (\g -> Map.alter (Just . owe g . Maybe.fromMaybe []) g) (owed w) guests})
      now <- State.gets lastTold >>= Link.telling (Map.fromList [(g, channel g) | g <- guests]) seat gs depth
      State.modify' (\w -> w {lastTold = now})

-- No deck's colours known.
uncoloured :: Text.Text -> Set.Set Color.Color
uncoloured = const Set.empty

-- The match-up a rejoining guest is told.
played :: [Link.Place]
played = [Link.MkPlace (Text.pack "Player 1") (Text.pack "Bears") (Just True) False Set.empty, Link.MkPlace (Text.pack "You") (Text.pack "Bears") (Just True) False Set.empty]

-- A guest rejoining, which connects the first time and not again, its page
-- reloaded.
rejoined :: (Monad m) => PlayerId.PlayerId -> State.StateT Wire m Bool
rejoined g = do
  State.modify' (\w -> w {rejoins = rejoins w + 1, pageSeen = Map.delete g (pageSeen w)})
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
resumedGame matchup = Game.run (S.fixtureRegistry []) GameSettings.plain Limits.standard [] (Map.fromList [(pid, Game.wired (S.fixtureRegistry []) resuming) | (pid, _) <- NonEmpty.toList matchup]) (S.seeded 1) Map.empty matchup

-- resumedGame with Bob's seat the search bot ('searching'), its answers
-- stored and kept alongside the page's (Game.remembered).
againstSearch :: (Monad m) => NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> State.StateT Resumed m (Either Text.Text (Result.Result, GameState.GameState))
againstSearch matchup =
  let seat pid = if pid == S.bob then Game.remembered nextStored (\reply -> State.modify' (\r -> r {kept = reply : kept r})) searching else Game.wired (S.fixtureRegistry []) resuming
   in Game.run (S.fixtureRegistry []) GameSettings.plain Limits.standard [] (Map.fromList [(pid, seat pid) | (pid, _) <- NonEmpty.toList matchup]) (S.seeded 1) Map.empty matchup

-- The search bot, at 2 samples cut off after 40 prompts, noting each search.
searching :: (Monad m) => Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT Resumed m))
searching = Answerer.MkAnswerer $ \prompted -> do
  Monad.when (Maybe.isJust (Protocol.requestFor prompted)) $ Game.underneath (State.modify' (\r -> r {searches = length (kept r) : searches r}))
  Answerer.unwrap (Search.search (S.fixtureRegistry []) budget Weights.neutral Search.known (pure []) (pure Search.blank) Search.unkept (pure ()) Search.unhurried) prompted
  where
    budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = SearchMethod.Flat}

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
      -- A reloaded page starts from what its seat has seen (Guest.play).
      Right again@(Link.MkStart _ seen') -> State.modify' (\w -> w {sentAgain = sentAgain w <> [again], pageSeen = Map.insert g seen' (pageSeen w)})
      Right again@(Link.MkMatchup _) -> State.modify' (\w -> w {sentAgain = sentAgain w <> [again]})
      Right line -> do
        heard <- Link.guest (page (Just g)) line
        case heard of
          Link.MkReply reply -> State.modify' (\w -> w {inbox = Map.insertWith (flip (<>)) g [Link.encode build reply] (inbox w)})
          Link.MkEnded {} -> State.modify' (\w -> w {ended = Map.insert g heard (ended w)})
          Link.MkWaitingOn seat shown depth -> State.modify' (\w -> w {waitedOn = Map.insertWith (<>) g [(seat, shown, depth)] (waitedOn w)})
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
page which request = State.modify' (\w -> w {asked = Map.insertWith (flip (<>)) which [seen request] (asked w), pageSeen = maybe id seenBy (Request.view request) (pageSeen w)}) >> pure (Just (Right (answer request)))

-- What each seat saw, with this view seen by its seat.
seenBy :: PlayerView.PlayerView -> Map.Map PlayerId.PlayerId Ending.Seen -> Map.Map PlayerId.PlayerId Ending.Seen
seenBy shown = Map.alter (Just . Ending.see shown . Maybe.fromMaybe Ending.unseen) (PlayerView.viewer shown)

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
