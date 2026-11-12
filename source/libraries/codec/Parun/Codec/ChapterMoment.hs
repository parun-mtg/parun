module Parun.Codec.ChapterMoment where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ChapterMoment as ChapterMoment

-- | Nullary tags, Parun.Codec.AbilityKind's shape and for its reason.
codec :: Codec.Codec ChapterMoment.ChapterMoment
codec = Arm.enum
