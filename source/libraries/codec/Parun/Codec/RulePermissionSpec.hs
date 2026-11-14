module Parun.Codec.RulePermissionSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.RulePermission as RulePermission
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.AimedAt as AimedAt
import qualified Parun.Types.AimedPlayers as AimedPlayers
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.RuleWaiver as RuleWaiver

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RulePermission" $ do
  -- Prison Barricade's kicked grant (CR 702.3b): "This creature".
  Spec.it s "MkRulePermission" $
    Common.assertCodec
      s
      RulePermission.codec
      (RulePermission.MkRulePermission (Affected.Matching Filter.IsSource) Nothing RuleWaiver.Defender Nothing)
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"IsSource\"}}} "
  Spec.it s "MkRulePermission with while" $
    Common.assertCodec
      s
      RulePermission.codec
      ( RulePermission.MkRulePermission
          (Affected.Matching Filter.IsSource)
          (Just (Condition.Compares (Compares.MkCompares (Quantity.IsMonarch (PlayerRef.Relative PlayerRelation.You)) Comparison.AtLeast (Quantity.Literal 1))))
          RuleWaiver.Defender
          Nothing
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"IsSource\"}},\"while\":{\"type\":\"Compares\",\"value\":{\"comparison\":\"AtLeast\",\"measured\":{\"type\":\"IsMonarch\",\"value\":{\"type\":\"Relative\",\"value\":\"You\"}},\"threshold\":{\"type\":\"Literal\",\"value\":1}}}} "
  -- Instill Energy (CR 302.6): "Enchanted creature".
  Spec.it s "MkRulePermission waiving the attack half of summoning sickness" $
    Common.assertCodec
      s
      RulePermission.codec
      (RulePermission.MkRulePermission Affected.Attached Nothing RuleWaiver.AttackSickness Nothing)
      " {\"affected\":{\"type\":\"Attached\"},\"waives\":\"AttackSickness\"} "
  -- Frenzied Saddlebrute (CR 508.1b): "can attack your opponents and
  -- planeswalkers your opponents control".
  Spec.it s "MkRulePermission bound to what the creature attacks" $
    Common.assertCodec
      s
      RulePermission.codec
      ( RulePermission.MkRulePermission
          Affected.Attached
          Nothing
          RuleWaiver.AttackSickness
          (Just (AimedAt.MkAimedAt (AimedPlayers.Scoped (PlayerScope.Related PlayerRelation.Opponent)) (Set.fromList [AttackTargetKind.OfPlayer, AttackTargetKind.OfPlaneswalker])))
      )
      " {\"affected\":{\"type\":\"Attached\"},\"against\":{\"defenders\":{\"type\":\"Scoped\",\"value\":{\"type\":\"Opponent\"}},\"kinds\":[\"OfPlayer\",\"OfPlaneswalker\"]},\"waives\":\"AttackSickness\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RulePermission.codec
