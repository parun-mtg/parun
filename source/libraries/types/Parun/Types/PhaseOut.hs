module Parun.Types.PhaseOut where

import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.ObjectRef as ObjectRef

-- | The payload of Parun.Types.Effect's PhaseOut arm: what phases out (CR
-- 702.26b), and for how long. No duration is CR 702.26a's untap-step schedule;
-- a MoveDuration is CR 610.4's "until", which declines the phase-out when the
-- event has already happened (CR 610.4b, 610.4c) and phases the permanent in
-- immediately after it. Parun.Engine.Monarch.crown marks only
-- GameState.movedUntil, so a crowning never ends a phase-out; nothing prints one.
data PhaseOut = MkPhaseOut
  { ref :: ObjectRef.ObjectRef,
    duration :: Maybe MoveDuration.MoveDuration
  }
  deriving (Eq, Ord, Show)
