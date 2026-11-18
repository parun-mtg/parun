module Parun.Codec.RollEnd where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RollEnd as RollEnd

codec :: Codec.Codec RollEnd.RollEnd
codec = Arm.enum
