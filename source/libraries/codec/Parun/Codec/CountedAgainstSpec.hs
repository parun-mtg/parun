module Parun.Codec.CountedAgainstSpec where

import qualified Parun.Codec.CountedAgainst as CountedAgainst
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CountedAgainst as CountedAgainst

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CountedAgainst" $ do
  Spec.it s "PaidFor" $
    Common.assertCodec
      s
      CountedAgainst.codec
      CountedAgainst.PaidFor
      " \"PaidFor\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CountedAgainst.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s CountedAgainst.codec
