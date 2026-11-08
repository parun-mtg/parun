module Parun.Codec.ThrowResultSpec where

import qualified Parun.Codec.ThrowResult as ThrowResult
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.ThrowResult as ThrowResult

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ThrowResult" $ do
  Spec.it s "CoinCameUp" $
    Common.assertCodec
      s
      ThrowResult.codec
      (ThrowResult.CoinCameUp CoinFace.Heads)
      " {\"type\":\"CoinCameUp\",\"value\":\"Heads\"} "
  Spec.it s "DieCameUp" $
    Common.assertCodec
      s
      ThrowResult.codec
      (ThrowResult.DieCameUp 3)
      " {\"type\":\"DieCameUp\",\"value\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ThrowResult.codec
