module Parun.Codec.AbilityActivatedTriggerSpec where

import qualified Parun.Codec.AbilityActivatedTrigger as AbilityActivatedTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityActivatedTrigger as AbilityActivatedTrigger
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AbilityActivatedTrigger" $ do
  Spec.it s "writes no kind key for an unnarrowed kind" $
    Common.assertCodec
      s
      AbilityActivatedTrigger.codec
      (AbilityActivatedTrigger.MkAbilityActivatedTrigger (Filter.HeldBy Holder.Controller PlayerRelation.You) Nothing)
      " {\"filter\":{\"type\":\"HeldBy\",\"value\":{\"holder\":\"Controller\",\"relation\":\"You\"}}} "
  Spec.it s "round-trips every field" $
    Common.assertCodec
      s
      AbilityActivatedTrigger.codec
      (AbilityActivatedTrigger.MkAbilityActivatedTrigger (Filter.HeldBy Holder.Controller PlayerRelation.Opponent) (Just AbilityKind.NonManaAbility))
      " {\"filter\":{\"type\":\"HeldBy\",\"value\":{\"holder\":\"Controller\",\"relation\":\"Opponent\"}},\"kind\":\"NonManaAbility\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityActivatedTrigger.codec
