module Parun.Codec.RuleFamily where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RuleFamily as RuleFamily

codec :: Codec.Codec RuleFamily.RuleFamily
codec = Arm.enum
