module Parun.Codec.RequiredBlockers where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RequiredBlockers as RequiredBlockers

codec :: Codec.Codec RequiredBlockers.RequiredBlockers
codec = Arm.enum
