module Parun.Codec.SpentOn where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.SpentOn as SpentOn

codec :: Codec.Codec SpentOn.SpentOn
codec = Arm.enum
