-- Covers Parun.Web.Link, Parun.Play.Protocol's bySeat and Parun.Web.Lobby's
-- invites: a game whose second seat is played from another page, over a
-- channel held in memory, and the agreement that starts it.
module Parun.RemoteSeatSpec where

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
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Web.Ending as Ending
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

  Spec.it s "a whole game in which the guest's seat is answered only through the remote exchange, and the guest sees only its own view" $ do
    matchup <- BotSpec.bears s registry
    (outcome, wire) <- State.runStateT (hosted matchup) (MkWire [] [] [] Nothing)
    case outcome of
      Right (Result.Won winner, _) -> do
        let verdict = if winner == S.bob then Ending.MkWon else Ending.MkLost
        told <- State.execStateT (Link.send channel (Link.MkEnd verdict [Text.pack "over"] Text.empty)) wire
        Spec.assertEqWith s "the guest is told how it ended" (ended told) (Just (Link.MkEnded verdict [Text.pack "over"] Text.empty))
      _ -> Spec.assertBool s False ("finished: " <> show (fmap fst outcome))
    Spec.assertBool s (not (null (hostAsked wire)) && all ((== S.alice) . fst) (hostAsked wire)) "the host's page answers only its own seat"
    Spec.assertBool s (not (null (guestAsked wire)) && all ((== S.bob) . fst) (guestAsked wire)) "the guest's page answers only its own seat"
    Spec.assertEqWith s "the guest is sent only its own seat's view" (ListUtils.nubOrd (Maybe.mapMaybe snd (guestAsked wire))) [S.bob]

-- Both pages and the channel between them: the lines the host has yet to
-- read, the seat of each request each page answered and whose view it
-- carried, and the end the guest was told.
data Wire = MkWire
  { inbox :: [Text.Text],
    hostAsked :: [(PlayerId.PlayerId, Maybe PlayerId.PlayerId)],
    guestAsked :: [(PlayerId.PlayerId, Maybe PlayerId.PlayerId)],
    ended :: Maybe Link.Heard
  }

build :: Text.Text
build = Text.pack "test"

-- The host's game: every seat over the line protocol, Player 2's requests
-- sent over the channel and Player 1's to the host's page.
hosted :: (Monad m) => NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> State.StateT Wire m (Either Text.Text (Result.Result, GameState.GameState))
hosted = Game.run (S.fixtureRegistry []) Nothing [] (Map.fromList [(pid, Game.wired (S.fixtureRegistry []) (Protocol.bySeat (Map.singleton S.bob (Link.remote channel)) hostPage)) | pid <- [S.alice, S.bob]]) (S.seeded 1) Map.empty

-- The host's end of the channel: each message rendered and read back as a
-- line, answered at once by the guest's page (Link.guest), whose reply waits
-- for the host to read.
channel :: (Monad m) => Link.Channel (State.StateT Wire m)
channel = Link.MkChannel send received
  where
    send message = case Link.decode build (Link.encode build message) of
      Left _ -> pure ()
      Right line -> do
        heard <- Link.guest guestPage line
        case heard of
          Link.MkReply reply -> State.modify' (\w -> w {inbox = inbox w <> [Link.encode build reply]})
          Link.MkEnded {} -> State.modify' (\w -> w {ended = Just heard})
          Link.MkIgnored -> pure ()

-- The next line the guest's page sent, read; Nothing once there is none.
received :: (Monad m) => State.StateT Wire m (Maybe Link.Message)
received = do
  w <- State.get
  case inbox w of
    line : rest -> State.put w {inbox = rest} >> pure (either (const Nothing) Just (Link.decode build line))
    [] -> pure Nothing

hostPage :: (Monad m) => Protocol.Exchange (State.StateT Wire m)
hostPage request = State.modify' (\w -> w {hostAsked = hostAsked w <> [seen request]}) >> pure (Just (Right (answer request)))

guestPage :: (Monad m) => Protocol.Exchange (State.StateT Wire m)
guestPage request = State.modify' (\w -> w {guestAsked = guestAsked w <> [seen request]}) >> pure (Just (Right (answer request)))

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
