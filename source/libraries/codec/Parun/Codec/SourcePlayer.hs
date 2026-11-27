module Parun.Codec.SourcePlayer where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.SourcePlayer as SourcePlayer

codec :: Codec.Codec SourcePlayer.SourcePlayer
codec = Arm.enum
