module Parun.Codec.PhaseAnchor where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PhaseAnchor as PhaseAnchor

codec :: Codec.Codec PhaseAnchor.PhaseAnchor
codec = Arm.enum
