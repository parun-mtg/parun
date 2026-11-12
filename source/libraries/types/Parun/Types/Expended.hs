module Parun.Types.Expended where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 700.14: a player paid a cost to cast a spell with mana -- who, and the
-- mana they had spent this turn to cast spells before and after paying it.
data Expended = MkExpended
  { player :: PlayerId.PlayerId,
    before :: Natural.Natural,
    after :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
