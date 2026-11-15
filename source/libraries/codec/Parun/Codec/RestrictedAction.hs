module Parun.Codec.RestrictedAction where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RestrictedAction as RestrictedAction

codec :: Codec.Codec RestrictedAction.RestrictedAction
codec = Arm.enum
