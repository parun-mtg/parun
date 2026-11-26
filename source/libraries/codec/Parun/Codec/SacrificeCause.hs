module Parun.Codec.SacrificeCause where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.SacrificeCause as SacrificeCause

codec :: Codec.Codec SacrificeCause.SacrificeCause
codec = Arm.enum
