-- | An action offered at priority (CR 116.2, 605.3a), in the one spelling both
-- the line protocol and a scenario read: fields for a client, and a line
-- derived from them for a scenario's Take and Offered view.
module Parun.Scenario.Offer where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Parun.Codec.Cost as Codec.Cost
import qualified Parun.Codec.FaceDownReason as Codec.FaceDownReason
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.TurnUpProcedure as Codec.TurnUpProcedure
import qualified Parun.Engine.EndEffect as EndEffect
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Scenario.Reply as Reply
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.Action as Action
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Reply as R

-- | An action as fields: its constructor, the object it acts on, then what
-- tells it from the constructor's other offers on that object: the face or
-- half a Play, Cast or Unlock names, a face-down Cast's reason (CR 708.6), an
-- Activate's place among its source's abilities (Projection.abilitiesOf, as a
-- scenario's Activation names it), and the choice a TurnFaceUp, Ignore or Plot
-- carries. An EndEffect's `offer` is its 1-based place among one source's
-- offers to one seat (CR 116.2c).
offer :: Reply.Namer -> GameState.GameState -> Action.Action -> R.Reply
offer namer gs action = R.Object ((Text.pack "action", R.Text (Text.pack kind)) : fmap (Bifunctor.first Text.pack) rest)
  where
    (kind, rest) = case action of
      Action.Pass -> ("Pass", [])
      Action.Play oid name -> ("PlayLand", on oid <> foldMap named name)
      Action.Cast oid name facing -> ("Cast", on oid <> named name <> faceDown facing)
      Action.Activate oid ability -> ("Activate", on oid <> [("ability", natural (Natural.length (takeWhile (/= ability) abilities))) | let abilities = Projection.abilitiesOf oid gs, ability `elem` abilities])
      Action.ActivateManaAbility oid -> ("ActivateManaAbility", on oid)
      Action.TurnFaceUp oid procedure -> ("TurnFaceUp", on oid <> [("procedure", Reply.encode (Reply.viaCodec Codec.TurnUpProcedure.codec) namer procedure)])
      Action.TurnConspiracyFaceUp oid -> ("TurnConspiracyFaceUp", on oid)
      Action.Unlock oid name -> ("Unlock", on oid <> named name)
      Action.DiscardFromHand oid -> ("DiscardFromHand", on oid)
      Action.Ignore oid ability -> ("Ignore", on oid <> [("abilityName", R.Text (AbilityName.unwrap ability))])
      Action.Plot oid cost -> ("Plot", on oid <> [("cost", Reply.encode (Reply.viaCodec (Codec.Cost.codec Codec.Keyword.codec)) namer cost)])
      Action.Foretell oid -> ("Foretell", on oid)
      Action.Suspend oid -> ("Suspend", on oid)
      Action.PutCompanionIntoHand -> ("PutCompanionIntoHand", [])
      Action.EndEffect oid resolution -> ("EndEffect", on oid <> [("offer", natural (Int.toNaturalSaturating (EndEffect.offerIndex oid resolution gs) + 1))])
      Action.MakeLaterPayment oid _ -> ("MakeLaterPayment", on oid)
      Action.PayLifeForMana life mana -> ("PayLifeForMana", [("life", natural life), ("mana", Reply.encode (Reply.viaCodec Codec.ManaType.codec) namer mana)])
      Action.RollPlanarDie -> ("RollPlanarDie", [])
    on :: ObjectId.ObjectId -> [(String, R.Reply)]
    on oid = [("object", Reply.encode Reply.object namer oid)]
    named name = [("name", R.Text (CardName.unwrap name))]
    faceDown facing = case facing of
      Facing.FaceUp -> []
      Facing.FaceDown state -> [("faceDown", Reply.encode (Reply.viaCodec Codec.FaceDownReason.codec) namer (FaceDownState.reason state))]
    natural = Reply.encode Reply.natural namer

-- | Each offer's line, as a scenario names it: its action and its object,
-- then each further field's value only where another offer of the same
-- action, agreeing on every field before it, differs there.
linesOf :: [R.Reply] -> [Text.Text]
linesOf offers = fmap lineOf offers
  where
    lineOf o =
      let pairs = pairsOf o
          kept = [word value | (i, (key, value)) <- zip [0 :: Int ..] pairs, key == Text.pack "action" || key == Text.pack "object" || differs (take i pairs) key value]
       in Text.unwords kept
    differs before key value = any (\other -> all (\(k, v) -> lookup k (pairsOf other) == Just v) before && lookup key (pairsOf other) /= Just value) offers
    pairsOf o = case o of
      R.Object pairs -> pairs
      _ -> []

-- | A field's value as a word of a line: text as itself, a number in digits,
-- anything else as compact JSON.
word :: R.Reply -> Text.Text
word value = case value of
  R.Text t -> t
  R.Number n -> Text.pack (show n)
  _ -> Common.render (Codec.Reply.toValue value)
