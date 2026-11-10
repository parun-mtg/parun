module Parun.Types.Prohibition where

import qualified Parun.Types.UntapScope as UntapScope

-- | What a stored per-object prohibition forbids: one CR 101.2 "can't" that a
-- resolution puts on the permanents it names (CR 611.2a). The classification
-- Parun.Engine.RestrictedCreatures.prohibited is asked by; each reader asks for one.
data Prohibition
  = -- | Its activated abilities can't be activated (CR 602.5).
    Activate
  | -- | It can't block (CR 509.1b).
    Block
  | -- | Counters can't be put on it (CR 122.6).
    Counters
  | -- | It stays phased out through its untap step (CR 702.26a).
    PhaseIn
  | -- | It can't be regenerated (CR 701.19c).
    Regenerate
  | -- | It doesn't untap in the scope given (CR 502.3, 701.26b).
    Untap UntapScope.UntapScope
  deriving (Eq, Ord, Show)
