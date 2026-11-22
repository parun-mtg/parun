module Parun.Types.DieResult where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.RollReading as RollReading

-- | CR 706.2: one number a die roll came out as, and which number it is,
-- beside the player the `player` parameter names -- the roller itself on
-- Parun.Types.GameEvent's DieResultSettled and RolledToVisit.
-- Parun.Types.RollTrigger is the watching side, a Parun.Types.PlayerRelation to
-- CR 109.5's "you" in its place.
data DieResult player = MkDieResult
  { roller :: player,
    result :: Natural.Natural,
    reading :: RollReading.RollReading
  }
  deriving (Eq, Ord, Show)
