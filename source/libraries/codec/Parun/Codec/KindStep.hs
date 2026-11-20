module Parun.Codec.KindStep where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.KindStep as KindStep

-- | Nullary tags, Parun.Codec.AbilityKind's shape and for its reason.
codec :: Codec.Codec KindStep.KindStep
codec = Arm.enum
