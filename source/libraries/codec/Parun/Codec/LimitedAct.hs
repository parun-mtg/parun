module Parun.Codec.LimitedAct where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.LimitedAct as LimitedAct

codec :: Codec.Codec LimitedAct.LimitedAct
codec = Arm.enum
