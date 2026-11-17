module Parun.Codec.BoundReadingSpec where

import qualified Parun.Codec.BoundReading as BoundReading
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.BoundReading as BoundReading

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BoundReading" $ do
  Spec.it s "BeforeMove" $
    Common.assertCodec
      s
      BoundReading.codec
      BoundReading.BeforeMove
      " \"BeforeMove\" "
  -- Parun.Codec.DamageKindSpec's reason.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s BoundReading.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s BoundReading.codec
