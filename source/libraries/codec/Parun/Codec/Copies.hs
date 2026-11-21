module Parun.Codec.Copies where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Copies as Copies

-- | Nullary tags, Parun.Codec.LoyaltyKind's shape.
codec :: Codec.Codec Copies.Copies
codec = Arm.enum
