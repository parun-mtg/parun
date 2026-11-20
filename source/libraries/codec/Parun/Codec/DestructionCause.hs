module Parun.Codec.DestructionCause where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.DestructionCause as DestructionCause

-- | Nullary tags, Parun.Codec.AbilityKind's shape. Runtime-only.
codec :: Codec.Codec DestructionCause.DestructionCause
codec = Arm.enum
