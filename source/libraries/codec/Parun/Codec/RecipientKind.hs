module Parun.Codec.RecipientKind where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RecipientKind as RecipientKind

codec :: Codec.Codec RecipientKind.RecipientKind
codec = Arm.enum
