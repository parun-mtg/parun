module Parun.Codec.RerollReach where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RerollReach as RerollReach

codec :: Codec.Codec RerollReach.RerollReach
codec = Arm.enum
