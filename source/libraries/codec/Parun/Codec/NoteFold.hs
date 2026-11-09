module Parun.Codec.NoteFold where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.NoteFold as NoteFold

codec :: Codec.Codec NoteFold.NoteFold
codec = Arm.enum
