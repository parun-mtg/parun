module Parun.Types.DieResult where

import qualified Numeric.Natural as Natural

-- | CR 706.2: one die's result, after every modifier, and its natural result,
-- beside the player the `player` parameter names -- the roller itself on
-- Parun.Types.GameEvent's DieResultSettled and RolledToVisit.
-- Parun.Types.RollTrigger is the watching side, a Parun.Types.PlayerRelation to
-- CR 109.5's "you" in its place.
data DieResult player = MkDieResult
  { roller :: player,
    result :: Natural.Natural,
    -- | CR 706.2's natural result, the face the die shows once its rerolls
    -- are done; Nothing where a coin stood in for the die (Pokey, the
    -- Scallywagg).
    natural :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)
