module Parun.Codec.ChoiceOrdinal where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ChoiceOrdinal as ChoiceOrdinal

codec :: Codec.Codec ChoiceOrdinal.ChoiceOrdinal
codec = Arm.enum
