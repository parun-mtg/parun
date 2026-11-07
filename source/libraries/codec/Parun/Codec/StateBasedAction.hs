module Parun.Codec.StateBasedAction where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.StateBasedAction as StateBasedAction

codec :: Codec.Codec StateBasedAction.StateBasedAction
codec = Arm.enum
