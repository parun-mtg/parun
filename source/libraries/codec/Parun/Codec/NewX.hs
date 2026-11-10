module Parun.Codec.NewX where

import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.NewX as NewX

codec :: Codec.Codec NewX.NewX
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Becomes" Quantity.codec NewX.Becomes (\x -> case x of NewX.Becomes y -> Just y; _ -> Nothing),
      Arm.payload "Multiplied" Common.natural NewX.Multiplied (\x -> case x of NewX.Multiplied y -> Just y; _ -> Nothing)
    ]

tagOf :: NewX.NewX -> String
tagOf x = case x of
  NewX.Becomes {} -> "Becomes"
  NewX.Multiplied {} -> "Multiplied"
