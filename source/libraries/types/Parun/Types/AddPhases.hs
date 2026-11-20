module Parun.Types.AddPhases where

import qualified Parun.Types.ExtraPhase as ExtraPhase
import qualified Parun.Types.PhaseAnchor as PhaseAnchor
import qualified Parun.Types.PhaseGrant as PhaseGrant

-- | CR 500.8: which phases an effect adds, the phase it adds them after, and
-- whom it says gets them.
data AddPhases = MkAddPhases
  { after :: PhaseAnchor.PhaseAnchor,
    -- | In written order.
    phases :: [ExtraPhase.ExtraPhase],
    grant :: PhaseGrant.PhaseGrant
  }
  deriving (Eq, Ord, Show)
