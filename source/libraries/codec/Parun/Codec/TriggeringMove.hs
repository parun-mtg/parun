module Parun.Codec.TriggeringMove where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.TriggeringMove as TriggeringMove

codec :: Codec.Codec TriggeringMove.TriggeringMove
codec = Arm.enum
