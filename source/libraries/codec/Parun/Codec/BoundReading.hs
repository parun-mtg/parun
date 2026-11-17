module Parun.Codec.BoundReading where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.BoundReading as BoundReading

codec :: Codec.Codec BoundReading.BoundReading
codec = Arm.enum
