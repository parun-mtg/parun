module Parun.Codec.AttackedTriggerSpec where

import qualified Parun.Codec.AttackedTrigger as AttackedTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackCount as AttackCount
import qualified Parun.Types.AttackMeasure as AttackMeasure
import qualified Parun.Types.AttackedSubject as AttackedSubject
import qualified Parun.Types.AttackedTrigger as AttackedTrigger
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AttackedTrigger" $ do
  -- Lulu, Stern Guardian's "whenever an opponent attacks you": the two keys hold
  -- DIFFERENT relations, so a codec that crossed them would not round-trip.
  Spec.it s "an attacker and an attacked subject" $
    Common.assertCodec
      s
      AttackedTrigger.codec
      (AttackedTrigger.MkAttackedTrigger (Just PlayerRelation.Opponent) (AttackedSubject.Related PlayerRelation.You) Nothing)
      " {\"attacker\":\"Opponent\",\"attacked\":{\"type\":\"Related\",\"value\":\"You\"}} "
  -- Curse of Vitality's "whenever enchanted player is attacked".
  Spec.it s "no attacker writes no key" $
    Common.assertCodec
      s
      AttackedTrigger.codec
      (AttackedTrigger.MkAttackedTrigger Nothing AttackedSubject.Enchanted Nothing)
      " {\"attacked\":{\"type\":\"Enchanted\"}} "
  -- Attuma, Atlantean Warlord's "whenever one or more Merfolk you control
  -- attack a player".
  Spec.it s "a count writes its attackers" $
    Common.assertCodec
      s
      AttackedTrigger.codec
      (AttackedTrigger.MkAttackedTrigger (Just PlayerRelation.You) (AttackedSubject.Related PlayerRelation.AnyPlayer) (Just (AttackCount.MkAttackCount AttackMeasure.Creatures 1 (Filter.HasSubtype Subtype.Merfolk))))
      " {\"attacker\":\"You\",\"attacked\":{\"type\":\"Related\",\"value\":\"AnyPlayer\"},\"attackers\":{\"floor\":1,\"filter\":{\"type\":\"HasSubtype\",\"value\":\"Merfolk\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackedTrigger.codec
