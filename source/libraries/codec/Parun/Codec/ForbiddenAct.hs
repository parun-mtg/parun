module Parun.Codec.ForbiddenAct where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ForbiddenAct as ForbiddenAct

codec :: Codec.Codec ForbiddenAct.ForbiddenAct
codec = Arm.enum
