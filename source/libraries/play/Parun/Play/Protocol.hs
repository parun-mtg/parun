{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | The line protocol: each prompt a seat answers goes out as one JSON line
-- (a 'Request.Request') and its answer comes back as one, read through
-- 'Reply.shapeOf'. An object is spelled @"$"@ and its id, a player by its id.
module Parun.Play.Protocol where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.Reference as Codec.Reference
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Decide as Decide
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Wording as Wording
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Prompt as Scenario.Prompt
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Scenario.Reply as Reply
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Label as Label
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Reference as Reference
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Scratch as Scratch
import qualified Text.Read as Read

-- | Answers each prompt a seat answers with the reply `send` gets for its
-- request (over lines through 'viaLines'), asking
-- again with the problem until the reply reads, is on offer and `judge` lets
-- it stand; every other prompt goes to `fallback`. `judge` is what the host
-- refuses that the engine cannot, with why (Parun.Play.Game.judgingNames).
over :: (Monad m) => (forall r. Asked.Asked GameState.GameState r -> r -> m (Either Text.Text r)) -> (Request.Request -> m (Either Text.Text R.Reply)) -> Answerer.Answerer (Asked.Asked GameState.GameState) m -> Answerer.Answerer (Asked.Asked GameState.GameState) m
over judge send fallback = Answerer.MkAnswerer $ \asked -> case requestFor asked of
  Nothing -> Answerer.unwrap fallback asked
  Just request ->
    let go pending = do
          sent <- send pending
          -- A withdrawal is on no offer, by design (withdrawalOf).
          let offered reply
                | Just reply == Question.withdraw (Request.question request) = decodeAnswer (Asked.prompt asked) reply
                | otherwise = decodeAnswer (Asked.prompt asked) reply >>= onOffer (Question.offer (Request.question request)) (Asked.prompt asked)
          judged <-
            either (pure . Left) (judge asked) $
              sent >>= offered >>= lawful named (Asked.game asked) (Asked.prompt asked)
          either (\problem -> go pending {Request.problem = Just problem}) pure judged
        named = maybe (const (Text.pack "a creature")) Wording.named (Request.view request)
     in go request

-- | An answer the prompt offers, amounts within the bounds `offer` states
-- where it asks for an amount per item (amountsFit). The engine reads one it
-- does not with a fallback of its own, which would choose for the player.
-- ProtocolSpec's Novijen Sages case is the proof.
onOffer :: R.Reply -> Prompt.Prompt r -> r -> Either Text.Text r
onOffer offer prompt answer
  | not (Scenario.withinOffer prompt answer) = Left (Text.pack "not an answer the prompt offers")
  | Just caps <- field "amounts" offer,
    not (maybe False (amountsFit offer) (amountsIn caps (encodeAnswer prompt answer))) =
      Left (Text.pack "amounts outside the bounds the prompt states")
  | otherwise = Right answer

-- | An answer the rules let stand: an illegal declaration of attackers (CR
-- 508.1c, 508.1d) or blockers (CR 509.1b, 509.1c) is refused with why, where
-- the engine's rewind would ask again with no reason and degrade a repeat to a
-- declaration the player did not make. Attackers are judged before CR 508.1b's
-- announcements, so refused only where no announcement makes them legal; the
-- engine then offers only announcements that keep them legal
-- (Combat.attemptAttackDeclaration). ProtocolSpec's Wind Drake, Bonded
-- Construct, Jace Beleren and Alluring Siren cases are the proof.
lawful :: (ObjectId.ObjectId -> Text.Text) -> GameState.GameState -> Prompt.Prompt r -> r -> Either Text.Text r
lawful named gs prompt answer = case prompt of
  Prompt.DeclareAttackers _ pid _
    | not (Combat.attackCreaturesAllowed pid (\_ _ -> True) answer gs) ->
        Left (Text.pack "the declaration of attackers is illegal")
  Prompt.DeclareBlockers _ pid _ _
    | not (Combat.legalBlockDeclaration pid answer gs) ->
        Left (Text.pack "the declaration of blockers is illegal" <> foldMap (\(blocker, attacker) -> Text.pack ": " <> named blocker <> Text.pack " can't block " <> named attacker) (Maybe.listToMaybe (Combat.forbiddenBlocks pid answer gs)))
  _ -> Right answer

-- | One line: no newline inside it, and none after.
render :: Request.Request -> Text.Text
render = Common.render . Codec.encode Codec.Request.codec

-- | A seat's side of the protocol as values: each request, and Nothing at the
-- end of input or else its reply, Left where the reply does not read. A
-- client in the engine's process answers so, skipping the lines' JSON.
type Exchange m = Request.Request -> m (Maybe (Either Text.Text R.Reply))

-- | Each request sent to its question's seat's exchange in `seats`, the rest
-- to `others`: the web client's page, and each seat a guest plays from
-- another browser (Parun.Web.Link.remote).
bySeat :: Map.Map PlayerId.PlayerId (Exchange m) -> Exchange m -> Exchange m
bySeat seats others request = Map.findWithDefault others (Question.seat (Request.question request)) seats request

-- | A game picked up where it left off: each request answered from `stored`
-- while it has a reply, the request passed to `replayed` meanwhile, then by
-- `live`, each reply that reads passed to `kept`. A game from the same seed
-- with the same replies asks the same requests (Game.run is pure), so the
-- replies kept so far are the whole game. RemoteSeatSpec's resumed game is
-- the proof.
resumed :: (Monad m) => m (Maybe R.Reply) -> (Request.Request -> m ()) -> (R.Reply -> m ()) -> Exchange m -> Exchange m
resumed stored replayed kept live request = do
  next <- stored
  case next of
    Just reply -> replayed request >> pure (Just (Right reply))
    Nothing -> do
      answered <- live request
      Monad.forM_ answered (either (const (pure ())) kept)
      pure answered

-- | An exchange over lines: each request rendered, and its reply line read.
overLines :: (Functor m) => (Text.Text -> m (Maybe Text.Text)) -> Exchange m
overLines send = fmap (fmap readReply) . send . render

-- | What 'over' sends through, over lines that never end.
viaLines :: (Functor m) => (Text.Text -> m Text.Text) -> Request.Request -> m (Either Text.Text R.Reply)
viaLines send = fmap readReply . send . render

-- | A reply line's reply, or why it does not read.
readReply :: Text.Text -> Either Text.Text R.Reply
readReply line = Common.parse line >>= Codec.Reply.fromValue

-- | What the seat answering this prompt is sent, or Nothing for a prompt no
-- seat answers.
requestFor :: Asked.Asked GameState.GameState r -> Maybe Request.Request
requestFor asked =
  fmap
    (\question -> Request.MkRequest (Just (PlayerView.viewFor (Question.seat question) (Asked.game asked))) question Nothing (depthOf asked))
    (questionOf (Asked.game asked) (Asked.prompt asked))

-- | CR 730.1a: how many games the question's game is nested inside, 0 for the
-- main game's (Request.depth).
depthOf :: Asked.Asked g r -> Natural
depthOf = Natural.length . Asked.enclosing

questionOf :: GameState.GameState -> Prompt.Prompt r -> Maybe Question.Question
questionOf gs prompt =
  fmap
    (\seat -> Question.MkQuestion seat (Scenario.Prompt.kindOf prompt) (PromptOffer.offerOf namer gs prompt) (Monad.mfilter (\subject -> subject /= seat && Decide.deciderFor subject gs == Decider.MkDecider seat) (Driver.subjectOf prompt)) (fmap (encodeAnswer prompt) (withdrawalOf gs prompt)))
    (Driver.deciderOf prompt)

-- | CR 734.1: the answer that takes back the spell being cast or the ability
-- being activated (Parun.Engine.Event.announcing), at a prompt of its own
-- announcement where one does. For CR 601.2c's targets it is the object
-- itself, which CR 115.5 makes an illegal target, so CR 601.2e (602.2b) rewinds
-- the announcement whatever the slots' counts. Offered only to the announcing
-- player's own slots: another chooser's (Parun.Types.TargetSlot's `chooser`)
-- is not theirs to withdraw.
withdrawalOf :: GameState.GameState -> Prompt.Prompt r -> Maybe r
withdrawalOf gs prompt = case prompt of
  Prompt.ChooseTargets _ chooser oid slots
    | Maybe.isJust (Scratch.keptFaceDown (GameState.scratch gs)),
      Projection.View.controllerOf oid gs == Just chooser,
      Just slot <- Set.lookupMin (Map.keysSet slots) ->
        Just (Map.singleton slot (Set.singleton (Recipient.ToObject oid)))
  _ -> Nothing

decodeAnswer :: Prompt.Prompt r -> R.Reply -> Either Text.Text r
decodeAnswer prompt = resolve . Reply.decode (Reply.shapeOf prompt)

encodeAnswer :: Prompt.Prompt r -> r -> R.Reply
encodeAnswer prompt = Reply.encode (Reply.shapeOf prompt) namer

namer :: Reply.Namer
namer =
  Reply.MkNamer
    (Reference.Labelled . Label.MkLabel . Text.pack . show . ObjectId.unwrap)
    (Label.MkLabel . Text.pack . show . PlayerId.unwrap)

resolve :: Reply.Needs a -> Either Text.Text a
resolve needs = case needs of
  Reply.Done a -> Right a
  Reply.Failed problem -> Left problem
  Reply.NeedObject reference k -> case reference of
    Reference.Labelled label | Just n <- idOf label -> resolve (k (ObjectId.MkObjectId n))
    _ -> Left (Text.pack "expected $ and an object's id but got " <> Codec.Reference.toText reference)
  Reply.NeedPlayer label k -> case idOf label of
    Just n -> resolve (k (PlayerId.MkPlayerId n))
    Nothing -> Left (Text.pack "expected a player's id but got " <> Label.unwrap label)

idOf :: Label.Label -> Maybe Natural
idOf label =
  let t = Label.unwrap label
   in if not (Text.null t) && Text.all Char.isDigit t then Read.readMaybe (Text.unpack t) else Nothing

-- | CR 103.1: the seat this seat chooses to take the first turn, of `seats`
-- in turn order, asked over the line before the game begins, so with no
-- view; asked again with the problem until the reply is one offered. Between
-- two seats it is PlayOrDraw, Play or Draw; among more, ChooseStarter, a
-- seat's id. Nothing at the end of input.
playOrDraw :: (Monad m) => Exchange m -> PlayerId.PlayerId -> NonEmpty.NonEmpty PlayerId.PlayerId -> m (Maybe PlayerId.PlayerId)
playOrDraw send seat seats = go Nothing
  where
    (kind, choices, expected)
      | length seats <= 2 = (Text.pack "PlayOrDraw", [(Text.pack "Play", seat), (Text.pack "Draw", next)], Text.pack "expected Play or Draw")
      | otherwise = (Text.pack "ChooseStarter", [(Text.pack (show (PlayerId.unwrap pid)), pid) | pid <- NonEmpty.toList seats], Text.pack "expected a seat's id")
    next = nextSeat seat seats
    question = Question.MkQuestion seat kind (R.Array (fmap (R.Text . fst) choices)) Nothing Nothing
    go problem = do
      sent <- send (Request.MkRequest Nothing question problem 0)
      case fmap (>>= chosen) sent of
        Nothing -> pure Nothing
        Just (Right pid) -> pure (Just pid)
        Just (Left again) -> go (Just again)
    chosen reply = case reply of
      R.Text word | Just pid <- lookup word choices -> Right pid
      _ -> Left expected

-- | The seat after `seat` in turn order among `seats`, the first after the
-- last.
nextSeat :: (Eq a) => a -> NonEmpty.NonEmpty a -> a
nextSeat seat seats = case drop 1 (NonEmpty.dropWhile (/= seat) seats) of
  after : _ -> after
  [] -> NonEmpty.head seats

-- | Each amount an amount-per-item offer asks for (amounts), in order: the
-- keys leading to it, an index for an item of a list, and the most it takes.
amountLeaves :: R.Reply -> [([Either Natural R.Reply], Natural)]
amountLeaves reply = case reply of
  R.Array items
    | all (Maybe.isJust . naturalOf) items -> [([Left i], n) | (i, Just n) <- zip [0 ..] (fmap naturalOf items)]
    | otherwise -> [(Right key : path, n) | R.Array [key, value] <- items, (path, n) <- amountLeaves value]
  _ -> foldMap (\n -> [([], n)]) (naturalOf reply)

-- | An answer's amounts in the order amountLeaves lists the offer's `caps`,
-- none where the answer leaves an item out; Nothing where it names one the
-- offer does not.
amountsIn :: R.Reply -> R.Reply -> Maybe [Natural]
amountsIn caps answer =
  let paths = fmap fst (amountLeaves caps)
      given = amountLeaves answer
   in if all ((`elem` paths) . fst) given then Just [Maybe.fromMaybe 0 (lookup path given) | path <- paths] else Nothing

-- | Whether amounts, one per item of the offer's @"amounts"@ in amountLeaves'
-- order, fit the offer (amounts): each at most its cap and at least its floor,
-- adding up within the least and most, from one permanent where "fromOne" says
-- so, of one kind where "oneKind" does, and each kind within its tally under
-- "counters". The web client's reader and onOffer both ask it.
amountsFit :: R.Reply -> [Natural] -> Bool
amountsFit offer typed =
  let leaves = foldMap amountLeaves (field "amounts" offer)
      floors = foldMap amountLeaves (field "floors" offer)
      floorOf path = Maybe.fromMaybe 0 (lookup path floors)
      supply = foldMap (Maybe.mapMaybe entryOf) (field "counters" offer >>= arrayOf)
      oneKind = field "oneKind" offer >>= naturalOf
      chosen = [(path, n) | ((path, _), n) <- zip leaves typed, n > 0]
      spent = sum typed
      kinds = ListUtils.nubOrd (Maybe.mapMaybe (lastKey . fst) chosen)
      firstKey path = case path of
        Right k : _ -> Just k
        _ -> Nothing
      lastKey path = case reverse path of
        Right k : _ -> Just k
        _ -> Nothing
   in length typed == length leaves
        && and (zipWith (\(path, cap) n -> n <= cap && n >= floorOf path) leaves typed)
        && all (spent >=) (field "least" offer >>= naturalOf)
        && all (spent <=) (field "most" offer >>= naturalOf)
        && (field "fromOne" offer /= Just (R.Boolean True) || length (ListUtils.nubOrd (Maybe.mapMaybe (firstKey . fst) chosen)) <= 1)
        && all (\n -> case kinds of [kind] -> spent == min n (maybe 0 snd (List.find ((== kind) . fst) supply)); _ -> False) oneKind
        && all (\(kind, held) -> sum [n | (path, n) <- chosen, lastKey path == Just kind] <= held) supply

field :: String -> R.Reply -> Maybe R.Reply
field key reply = case reply of
  R.Object pairs -> List.lookup (Text.pack key) pairs
  _ -> Nothing

arrayOf :: R.Reply -> Maybe [R.Reply]
arrayOf reply = case reply of
  R.Array xs -> Just xs
  _ -> Nothing

-- | A map's entry, as the line protocol writes one.
entryOf :: R.Reply -> Maybe (R.Reply, Natural)
entryOf reply = case reply of
  R.Array [key, value] -> fmap ((,) key) (naturalOf value)
  _ -> Nothing

naturalOf :: R.Reply -> Maybe Natural
naturalOf reply = case reply of
  R.Number n -> Integer.toNatural n
  _ -> Nothing
