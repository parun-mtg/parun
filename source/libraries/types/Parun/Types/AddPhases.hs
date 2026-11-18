module Parun.Types.AddPhases where

import qualified Parun.Types.ExtraPhase as ExtraPhase
import qualified Parun.Types.PhaseAnchor as PhaseAnchor

-- | CR 500.8: which phases an effect adds, and the phase it adds them after.
data AddPhases = MkAddPhases
  { after :: PhaseAnchor.PhaseAnchor,
    -- | In written order.
    phases :: [ExtraPhase.ExtraPhase]
  }
  deriving (Eq, Ord, Show)
