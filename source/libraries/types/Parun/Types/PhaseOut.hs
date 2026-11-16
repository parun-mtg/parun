module Parun.Types.PhaseOut where

import qualified Parun.Types.MoveDuration as MoveDuration
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TapState as TapState

-- | The payload of Parun.Types.Effect's PhaseOut arm: what phases out (CR
-- 702.26b), and for how long. No duration is CR 702.26a's untap-step schedule;
-- a MoveDuration is CR 610.4's "until", which declines the phase-out when the
-- event has already happened (CR 610.4b, 610.4c) and phases the permanent in
-- immediately after it. Parun.Engine.Monarch.crown marks only
-- GameState.movedUntil, so a crowning never ends a phase-out; nothing prints one.
--
-- `slot` is where the count of named permanents that actually phased out is
-- written, for a later effect of the same resolution (Out of Time's "for each
-- creature that phased out this way"), Destroy's `slot`.
--
-- `phasesIn` is the status a named permanent takes as CR 610.4's second effect
-- phases it in (Oubliette's "tap that creature as it phases in this way"); no
-- tap or untap event is recorded. Read only by a duration's phase-in.
data PhaseOut = MkPhaseOut
  { ref :: ObjectRef.ObjectRef,
    duration :: Maybe MoveDuration.MoveDuration,
    slot :: Maybe SlotName.SlotName,
    phasesIn :: Maybe TapState.TapState
  }
  deriving (Eq, Ord, Show)
