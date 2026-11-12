module Parun.Types.PlayerExpends where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 700.14: "whenever [a player] expend[s] N" -- whose spending counts, and
-- the N.
data PlayerExpends = MkPlayerExpends
  { player :: PlayerRelation.PlayerRelation,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
