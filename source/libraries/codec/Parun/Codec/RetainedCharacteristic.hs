module Parun.Codec.RetainedCharacteristic where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RetainedCharacteristic as RetainedCharacteristic

codec :: Codec.Codec RetainedCharacteristic.RetainedCharacteristic
codec = Arm.enum
