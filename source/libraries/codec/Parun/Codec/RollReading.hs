module Parun.Codec.RollReading where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RollReading as RollReading

codec :: Codec.Codec RollReading.RollReading
codec = Arm.enum
