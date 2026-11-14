module Parun.Codec.CountedAgainst where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CountedAgainst as CountedAgainst

codec :: Codec.Codec CountedAgainst.CountedAgainst
codec = Arm.enum
