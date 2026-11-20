module Parun.Codec.NextWindow where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.NextWindow as NextWindow

codec :: Codec.Codec NextWindow.NextWindow
codec = Arm.enum
