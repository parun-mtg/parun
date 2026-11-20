module Parun.Codec.PhaseGrant where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PhaseGrant as PhaseGrant

codec :: Codec.Codec PhaseGrant.PhaseGrant
codec = Arm.enum
