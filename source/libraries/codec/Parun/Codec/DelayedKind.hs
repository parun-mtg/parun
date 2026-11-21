module Parun.Codec.DelayedKind where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.DelayedKind as DelayedKind

codec :: Codec.Codec DelayedKind.DelayedKind
codec = Arm.enum
