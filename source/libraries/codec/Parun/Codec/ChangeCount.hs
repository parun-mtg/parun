module Parun.Codec.ChangeCount where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ChangeCount as ChangeCount

codec :: Codec.Codec ChangeCount.ChangeCount
codec = Arm.enum
