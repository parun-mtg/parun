module Parun.Codec.RuleWaiver where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RuleWaiver as RuleWaiver

-- | Nullary tags, Parun.Codec.AbilityKind's shape.
codec :: Codec.Codec RuleWaiver.RuleWaiver
codec = Arm.enum
