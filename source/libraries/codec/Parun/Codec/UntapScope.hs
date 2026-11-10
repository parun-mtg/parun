module Parun.Codec.UntapScope where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.UntapScope as UntapScope

codec :: Codec.Codec UntapScope.UntapScope
codec = Arm.enum
