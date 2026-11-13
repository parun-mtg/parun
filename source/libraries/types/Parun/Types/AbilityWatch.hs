module Parun.Types.AbilityWatch where

import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.WatchedAbility as WatchedAbility

-- | CR 603.3b: "whenever [a triggered ability] of [a permanent a related player
-- controls]" triggers or resolves -- which abilities, and whose.
data AbilityWatch = MkAbilityWatch
  { ability :: WatchedAbility.WatchedAbility,
    controller :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)
