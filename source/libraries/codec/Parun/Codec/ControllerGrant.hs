module Parun.Codec.ControllerGrant where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ControllerGrant as ControllerGrant

codec :: Codec.Codec ControllerGrant.ControllerGrant
codec = Arm.enum
