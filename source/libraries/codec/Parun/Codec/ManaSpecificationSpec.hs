module Parun.Codec.ManaSpecificationSpec where

import qualified Parun.Codec.ManaSpecification as ManaSpecification
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ManaSpecification as ManaSpecification
import qualified Parun.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaSpecification" $ do
  Spec.it s "AnyMana" $
    Common.assertCodec
      s
      ManaSpecification.codec
      ManaSpecification.AnyMana
      " {\"type\":\"AnyMana\"} "
  Spec.it s "ChosenColor" $
    Common.assertCodec
      s
      ManaSpecification.codec
      ManaSpecification.ChosenColor
      " {\"type\":\"ChosenColor\"} "
  Spec.it s "OfType Colored" $
    Common.assertCodec
      s
      ManaSpecification.codec
      (ManaSpecification.OfType (ManaType.Colored Color.Red))
      " {\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":\"Red\"}} "
  Spec.it s "OfType Colorless" $
    Common.assertCodec
      s
      ManaSpecification.codec
      (ManaSpecification.OfType ManaType.Colorless)
      " {\"type\":\"OfType\",\"value\":{\"type\":\"Colorless\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaSpecification.codec
