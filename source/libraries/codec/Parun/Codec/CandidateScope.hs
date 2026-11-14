module Parun.Codec.CandidateScope where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CandidateScope as CandidateScope

codec :: Codec.Codec CandidateScope.CandidateScope
codec = Arm.enum
