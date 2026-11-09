module Parun.Codec.AbilityViewSpec where

import qualified Parun.Codec.AbilityView as AbilityView
import qualified Parun.Codec.TriggeredAbilitySourceSpec as TriggeredAbilitySourceSpec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityView as AbilityView
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.StackedAbility as StackedAbility

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AbilityView" $ do
  Spec.it s "a known source" $
    Common.assertCodec s AbilityView.codec (AbilityView.MkAbilityView (Just (ObjectId.MkObjectId 5)) triggered) (" {\"source\":5," <> json <> "} ")
  Spec.it s "an unknown source" $
    Common.assertCodec s AbilityView.codec (AbilityView.MkAbilityView Nothing triggered) (" {\"source\":null," <> json <> "} ")
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityView.codec
  where
    triggered = StackedAbility.Triggered TriggeredAbilitySourceSpec.ability
    json = "\"ability\":{\"type\":\"Triggered\",\"value\":{\"condition\":{\"type\":\"Enters\",\"value\":{}},\"modal\":{\"modes\":[{}]}}}"
