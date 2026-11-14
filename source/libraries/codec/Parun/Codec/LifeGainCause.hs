module Parun.Codec.LifeGainCause where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.LifeGainCause as LifeGainCause

codec :: Codec.Codec LifeGainCause.LifeGainCause
codec = Arm.enum
