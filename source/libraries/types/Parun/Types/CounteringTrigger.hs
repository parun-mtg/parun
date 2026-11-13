module Parun.Types.CounteringTrigger where

import qualified Parun.Types.CounteredKind as CounteredKind
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 701.6a: what was countered, who controlled what countered it (CR
-- 405.4, as of the countering), and who had cast the countered spell (CR
-- 603.10e).
data CounteringTrigger = MkCounteringTrigger
  { countered :: CounteredKind.CounteredKind,
    counterer :: PlayerRelation.PlayerRelation,
    -- | Only AnyPlayer admits what nobody cast: an ability, or a copy (CR
    -- 707.10).
    caster :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)

-- | "Whenever a spell is countered": any spell, whoever countered it and
-- whoever cast it. The codec's defaults.
defaultValue :: CounteringTrigger
defaultValue =
  MkCounteringTrigger
    { countered = CounteredKind.Spell,
      counterer = PlayerRelation.AnyPlayer,
      caster = PlayerRelation.AnyPlayer
    }
