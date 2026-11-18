module Parun.Codec.RetainedCharacteristicSpec where

import qualified Parun.Codec.RetainedCharacteristic as RetainedCharacteristic
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RetainedCharacteristic as RetainedCharacteristic

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RetainedCharacteristic" $ do
  Spec.it s "Colors" $
    Common.assertCodec
      s
      RetainedCharacteristic.codec
      RetainedCharacteristic.Colors
      " \"Colors\" "
  Spec.it s "Supertypes" $
    Common.assertCodec
      s
      RetainedCharacteristic.codec
      RetainedCharacteristic.Supertypes
      " \"Supertypes\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RetainedCharacteristic.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RetainedCharacteristic.codec
