module Parun.Codec.AttackMeasure where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.AttackMeasure as AttackMeasure

codec :: Codec.Codec AttackMeasure.AttackMeasure
codec = Arm.enum
