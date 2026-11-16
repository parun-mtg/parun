module Parun.Codec.CostedSpecialAction where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CostedSpecialAction as CostedSpecialAction

codec :: Codec.Codec CostedSpecialAction.CostedSpecialAction
codec = Arm.enum
