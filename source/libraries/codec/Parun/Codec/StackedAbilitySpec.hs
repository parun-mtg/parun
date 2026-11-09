module Parun.Codec.StackedAbilitySpec where

import qualified Parun.Codec.ActivatedAbilitySourceSpec as ActivatedAbilitySourceSpec
import qualified Parun.Codec.StackedAbility as StackedAbility
import qualified Parun.Codec.TriggeredAbilitySourceSpec as TriggeredAbilitySourceSpec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.StackedAbility as StackedAbility

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.StackedAbility" $ do
  Spec.it s "an activated ability" $
    Common.assertCodec s StackedAbility.codec (StackedAbility.Activated ActivatedAbilitySourceSpec.ability) " {\"type\":\"Activated\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"modal\":{\"modes\":[{}]}}} "
  Spec.it s "a triggered ability" $
    Common.assertCodec s StackedAbility.codec (StackedAbility.Triggered TriggeredAbilitySourceSpec.ability) " {\"type\":\"Triggered\",\"value\":{\"condition\":{\"type\":\"Enters\",\"value\":{}},\"modal\":{\"modes\":[{}]}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s StackedAbility.codec
