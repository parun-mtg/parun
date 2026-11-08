module Parun.Types.Tally where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Result as Result

-- | What `parun match` has counted over the games played so far.
data Tally = MkTally
  { -- | How many games ended in each result.
    results :: Map.Map Result.Result Natural.Natural,
    -- | How many games stopped before a result.
    stopped :: Natural.Natural,
    -- | The turns the games with a result took, summed.
    turns :: Natural.Natural
  }
  deriving (Eq, Show)

-- | No games yet.
empty :: Tally
empty = MkTally {results = Map.empty, stopped = 0, turns = 0}
