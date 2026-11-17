module Parun.Codec.ChoiceDistinct where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ChoiceDistinct as ChoiceDistinct

codec :: Codec.Codec ChoiceDistinct.ChoiceDistinct
codec = Arm.enum
