module Parun.Codec.AttackWindow where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.AttackWindow as AttackWindow

codec :: Codec.Codec AttackWindow.AttackWindow
codec = Arm.enum
