module Parun.Types.Limits where

import qualified Numeric.Natural as Natural

-- | How large a game may grow before the engine stops it growing further:
-- host limits the CR knows nothing of, which keep a board small enough to
-- play (see docs/decisions.md, "What a game does at a host limit").
newtype Limits = MkLimits
  { -- | The most objects a game may hold, in every zone and on the stack,
    -- before a player with priority is offered only Pass
    -- (Parun.Engine.Limit.offered).
    objects :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | The limits every game is started with unless its host sets others.
standard :: Limits
standard = MkLimits {objects = 1000}
