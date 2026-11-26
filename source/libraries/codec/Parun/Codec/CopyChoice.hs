module Parun.Codec.CopyChoice where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CopyChoice as CopyChoice

codec :: Codec.Codec CopyChoice.CopyChoice
codec = Arm.enum
