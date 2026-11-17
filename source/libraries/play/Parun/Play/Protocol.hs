{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | The line protocol: each prompt a seat answers goes out as one JSON line
-- (a 'Request.Request') and its answer comes back as one, read through
-- 'Reply.shapeOf'. An object is spelled @"$"@ and its id, a player by its id.
module Parun.Play.Protocol where

import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.Reference as Codec.Reference
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Extra.Integer as Integer
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Render as Render
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Prompt as Scenario.Prompt
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Scenario.Reply as Reply
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Label as Label
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reference as Reference
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
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
          judged <-
            either (pure . Left) (judge asked) $
              sent >>= decodeAnswer (Asked.prompt asked) >>= onOffer (Question.offer (Request.question request)) (Asked.prompt asked) >>= lawful named (Asked.game asked) (Asked.prompt asked)
          either (\problem -> go pending {Request.problem = Just problem}) pure judged
        named = maybe (const (Text.pack "a creature")) Render.named (Request.view request)
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
    (\question -> Request.MkRequest (Just (PlayerView.viewFor (Question.seat question) (Asked.game asked))) question Nothing)
    (questionOf (Asked.game asked) (Asked.prompt asked))

questionOf :: GameState.GameState -> Prompt.Prompt r -> Maybe Question.Question
questionOf gs prompt =
  fmap
    (\seat -> Question.MkQuestion seat (Scenario.Prompt.kindOf prompt) (PromptOffer.offerOf namer gs prompt))
    (Driver.deciderOf prompt)

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

-- | CR 103.1: whether this seat plays first rather than draws, asked over the
-- line before the game begins, so with no view; asked again with the problem
-- until the reply is Play or Draw. Nothing at the end of input.
playOrDraw :: (Monad m) => Exchange m -> PlayerId.PlayerId -> m (Maybe Bool)
playOrDraw send seat = go Nothing
  where
    choices = [(Text.pack "Play", True), (Text.pack "Draw", False)]
    question = Question.MkQuestion seat (Text.pack "PlayOrDraw") (R.Array (fmap (R.Text . fst) choices))
    go problem = do
      sent <- send (Request.MkRequest Nothing question problem)
      case fmap (>>= chosen) sent of
        Nothing -> pure Nothing
        Just (Right plays) -> pure (Just plays)
        Just (Left again) -> go (Just again)
    chosen reply = case reply of
      R.Text word | Just plays <- lookup word choices -> Right plays
      _ -> Left (Text.pack "expected Play or Draw")

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
-- "counters". The text client's reader and onOffer both ask it.
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
