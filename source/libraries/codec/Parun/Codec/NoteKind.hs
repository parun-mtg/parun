module Parun.Codec.NoteKind where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.NoteKind as NoteKind

codec :: Codec.Codec NoteKind.NoteKind
codec = Arm.enum
