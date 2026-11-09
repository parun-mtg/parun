module Parun.Codec.Parity where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Parity as Parity

codec :: Codec.Codec Parity.Parity
codec = Arm.enum
