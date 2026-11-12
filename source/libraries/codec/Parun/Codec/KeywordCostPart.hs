module Parun.Codec.KeywordCostPart where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.KeywordCostPart as KeywordCostPart

-- | Nullary tags, Parun.Codec.AbilityKind's shape.
codec :: Codec.Codec KeywordCostPart.KeywordCostPart
codec = Arm.enum
