module Parun.Codec.PendingOrigin where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.PendingOrigin as PendingOrigin

codec :: Codec.Codec PendingOrigin.PendingOrigin
codec = Arm.enum
