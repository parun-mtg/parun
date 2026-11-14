module Parun.Codec.GraveyardCostDestination where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination

codec :: Codec.Codec GraveyardCostDestination.GraveyardCostDestination
codec = Arm.enum
