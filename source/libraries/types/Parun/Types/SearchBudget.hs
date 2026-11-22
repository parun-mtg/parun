module Parun.Types.SearchBudget where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.SearchMethod as SearchMethod

-- | How hard the search bot (Parun.Play.Search) looks at a decision, and
-- how it plays each playout out.
data SearchBudget = MkSearchBudget
  { -- | The playouts of each option, each in a world of its own.
    samples :: Natural.Natural,
    -- | The turns a playout may begin after the decision's before it is cut
    -- off and its position scored.
    turns :: Natural.Natural,
    -- | The prompts a playout is answered before it is cut off and its
    -- position scored.
    prompts :: Natural.Natural,
    -- | The worlds dealt once and weighed by how likely the opponents' passes
    -- were in each, every playout's world drawn among them; none, each
    -- playout's world dealt fresh.
    candidates :: Natural.Natural,
    -- | Who answers every seat's prompts in a playout.
    playout :: Playout.Playout,
    -- | How the playouts are spent across the options.
    method :: SearchMethod.SearchMethod
  }
  deriving (Eq, Show)
