module Parun.Codec.ExileMark where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ExileMark as ExileMark

codec :: Codec.Codec ExileMark.ExileMark
codec = Arm.enum
