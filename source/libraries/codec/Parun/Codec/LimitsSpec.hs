module Parun.Codec.LimitsSpec where

import qualified Parun.Codec.Limits as Limits
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Limits as Limits

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Limits" $ do
  Spec.it s "an object limit" $
    Common.assertCodec s Limits.codec Limits.MkLimits {Limits.objects = 12} " {\"objects\":12} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Limits.codec
