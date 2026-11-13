module Parun.Codec.ControlSpan where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ControlSpan as ControlSpan

-- | Nullary tags, Parun.Codec.AbilityKind's shape and for its reason.
codec :: Codec.Codec ControlSpan.ControlSpan
codec = Arm.enum
