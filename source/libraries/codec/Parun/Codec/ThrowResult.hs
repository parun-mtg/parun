module Parun.Codec.ThrowResult where

import qualified Parun.Codec.CoinFace as CoinFace
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.ThrowResult as ThrowResult

codec :: Codec.Codec ThrowResult.ThrowResult
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "CoinCameUp" CoinFace.codec ThrowResult.CoinCameUp (\x -> case x of ThrowResult.CoinCameUp y -> Just y; _ -> Nothing),
      Arm.payload "DieCameUp" Common.natural ThrowResult.DieCameUp (\x -> case x of ThrowResult.DieCameUp y -> Just y; _ -> Nothing)
    ]

tagOf :: ThrowResult.ThrowResult -> String
tagOf x = case x of
  ThrowResult.CoinCameUp {} -> "CoinCameUp"
  ThrowResult.DieCameUp {} -> "DieCameUp"
