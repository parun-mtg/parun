module Parun.Codec.InterveningCheck where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.InterveningCheck as InterveningCheck

codec :: Codec.Codec InterveningCheck.InterveningCheck
codec = Arm.enum
