module Parun.Types.SearchBudget where

import qualified Numeric.Natural as Natural

-- | How hard the search bot (Parun.Play.Search) looks at a decision.
data SearchBudget = MkSearchBudget
  { -- | The playouts of each option, each in a world of its own.
    samples :: Natural.Natural,
    -- | The prompts a playout is answered before it is cut off and scored a
    -- draw.
    prompts :: Natural.Natural
  }
  deriving (Eq, Show)
