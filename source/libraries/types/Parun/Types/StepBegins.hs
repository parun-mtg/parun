module Parun.Types.StepBegins where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.TurnScope as TurnScope

-- | CR 603.2's turn-structure trigger: which step or phase beginning fires the
-- ability, and whose turns count.
data StepBegins = MkStepBegins
  { phase :: Phase.Phase,
    -- | Which occurrence this turn, counting every main phase together (CR
    -- 505.1b) and any other step by itself: Just 2 for "your second main
    -- phase", Just 1 for "first upkeep each turn" (CR 500.9), Nothing for a
    -- card naming every occurrence ("each postcombat main phase"). The two are
    -- different cards in a turn holding an extra main phase or step.
    ordinal :: Maybe Natural.Natural,
    -- | CR 603.2b, with CR 109.5's "your" versus an unscoped "each". Not a
    -- default a card may omit -- "at the beginning of the end step" and "at the
    -- beginning of YOUR end step" are different cards.
    scope :: TurnScope.TurnScope
  }
  deriving (Eq, Ord, Show)
