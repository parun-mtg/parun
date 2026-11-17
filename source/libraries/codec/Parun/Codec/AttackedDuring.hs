module Parun.Codec.AttackedDuring where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.AttackedDuring as AttackedDuring

codec :: Codec.Codec AttackedDuring.AttackedDuring
codec = Arm.enum
