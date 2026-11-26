module Parun.Codec.PaidResource where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PaidResource as PaidResource

codec :: Codec.Codec PaidResource.PaidResource
codec = Arm.enum
