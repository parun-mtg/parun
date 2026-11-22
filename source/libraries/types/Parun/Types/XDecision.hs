module Parun.Types.XDecision where

import qualified Numeric.Natural as Natural

-- | CR 107.3a / 601.2b: a player's answer to a variable's announcement -- the
-- value of X, or a count a cost announces beside it.
--
-- Its own type rather than a bare Natural, so the announcement admits a typed
-- decline: a value can be refused only where the card or the board refuses
-- it, and the bound Prompt.ChooseX carries is advisory.
data XDecision
  = -- | Announces this value.
    Announces Natural.Natural
  | -- | Takes the spell or ability back: the announcement rewinds (CR 734.1,
    -- 601.2e).
    Withdraws
  deriving (Eq, Ord, Show)
