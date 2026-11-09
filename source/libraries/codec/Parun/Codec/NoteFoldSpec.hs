module Parun.Codec.NoteFoldSpec where

import qualified Parun.Codec.NoteFold as NoteFold
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.NoteFold as NoteFold

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.NoteFold" $ do
  Spec.it s "Total" $
    Common.assertCodec
      s
      NoteFold.codec
      NoteFold.Total
      " \"Total\" "
  Spec.it s "Highest" $
    Common.assertCodec
      s
      NoteFold.codec
      NoteFold.Highest
      " \"Highest\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s NoteFold.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s NoteFold.codec
