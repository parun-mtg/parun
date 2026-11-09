module Parun.Codec.AttackMeasureSpec where

import qualified Parun.Codec.AttackMeasure as AttackMeasure
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackMeasure as AttackMeasure

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AttackMeasure" $ do
  Spec.it s "TotalPower" $
    Common.assertCodec
      s
      AttackMeasure.codec
      AttackMeasure.TotalPower
      " \"TotalPower\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s AttackMeasure.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackMeasure.codec
