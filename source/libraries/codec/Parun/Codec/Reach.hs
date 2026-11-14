module Parun.Codec.Reach where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Reach as Reach

codec :: Codec.Codec Reach.Reach
codec = Arm.enum
