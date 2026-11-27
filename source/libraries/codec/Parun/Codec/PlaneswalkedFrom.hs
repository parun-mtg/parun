module Parun.Codec.PlaneswalkedFrom where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PlaneswalkedFrom as PlaneswalkedFrom

codec :: Codec.Codec PlaneswalkedFrom.PlaneswalkedFrom
codec = Arm.enum
