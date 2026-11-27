module Parun.Codec.PlaneswalkAway where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PlaneswalkAway as PlaneswalkAway

codec :: Codec.Codec PlaneswalkAway.PlaneswalkAway
codec = Arm.enum
