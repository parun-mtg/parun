module Parun.Codec.ColorlessOption where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ColorlessOption as ColorlessOption

codec :: Codec.Codec ColorlessOption.ColorlessOption
codec = Arm.enum
