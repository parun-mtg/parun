module Parun.Codec.CostPortion where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CostPortion as CostPortion

codec :: Codec.Codec CostPortion.CostPortion
codec = Arm.enum
