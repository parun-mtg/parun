module Parun.Types.PhaseAnchor where

-- | CR 500.8: the phase an effect adds phases directly after.
data PhaseAnchor
  = -- | The phase the effect resolves in ("after this main phase").
    ThisPhase
  | -- | CR 505.1b: the turn's second main phase, counting main phases that
    -- have occurred this turn (World at War).
    SecondMainPhase
  deriving (Bounded, Enum, Eq, Ord, Show)
