module Parun.Codec.Legality where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Legality as Legality

codec :: Codec.Codec Legality.Legality
codec = Arm.enum
