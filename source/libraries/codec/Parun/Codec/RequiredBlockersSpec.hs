module Parun.Codec.RequiredBlockersSpec where

import qualified Parun.Codec.RequiredBlockers as RequiredBlockers
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RequiredBlockers as RequiredBlockers

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RequiredBlockers" $ do
  Spec.it s "ExactlyOne" $
    Common.assertCodec
      s
      RequiredBlockers.codec
      RequiredBlockers.ExactlyOne
      " \"ExactlyOne\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RequiredBlockers.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RequiredBlockers.codec
