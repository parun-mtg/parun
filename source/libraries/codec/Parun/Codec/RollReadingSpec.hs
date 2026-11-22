module Parun.Codec.RollReadingSpec where

import qualified Parun.Codec.RollReading as RollReading
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RollReading as RollReading

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RollReading" $ do
  Spec.it s "NaturalResult" $
    Common.assertCodec
      s
      RollReading.codec
      RollReading.NaturalResult
      " \"NaturalResult\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RollReading.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s RollReading.codec
