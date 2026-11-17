module Parun.Codec.RollKind where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RollKind as RollKind

codec :: Codec.Codec RollKind.RollKind
codec = Arm.enum
