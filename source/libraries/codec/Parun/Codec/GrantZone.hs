module Parun.Codec.GrantZone where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.GrantZone as GrantZone

codec :: Codec.Codec GrantZone.GrantZone
codec = Arm.enum
