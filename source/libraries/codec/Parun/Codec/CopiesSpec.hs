module Parun.Codec.CopiesSpec where

import qualified Parun.Codec.Copies as Copies
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Copies as Copies

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Copies" $ do
  Spec.it s "OnlyCopies" $
    Common.assertCodec
      s
      Copies.codec
      Copies.OnlyCopies
      " \"OnlyCopies\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Copies.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s Copies.codec
