module Parun.Codec.EntryWindow where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.EntryWindow as EntryWindow

codec :: Codec.Codec EntryWindow.EntryWindow
codec = Arm.enum
