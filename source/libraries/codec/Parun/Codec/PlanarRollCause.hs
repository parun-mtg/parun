module Parun.Codec.PlanarRollCause where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PlanarRollCause as PlanarRollCause

codec :: Codec.Codec PlanarRollCause.PlanarRollCause
codec = Arm.enum
