module Parun.Codec.Copyability where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Copyability as Copyability

-- | Nullary tags, derived from the type by Arm.enum: @"CantBeCopied"@.
codec :: Codec.Codec Copyability.Copyability
codec = Arm.enum
