module Parun.Types.Snapshots where

import qualified Data.Map.Strict as Map
import qualified Parun.Types.BattlefieldCandidate as BattlefieldCandidate
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.LastKnown as LastKnown
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics

-- | CR 603.10 / 608.2h: the game as it stood at earlier moments, held whole as
-- GameState.snapshots, `state` being the whole game a board snapshot keeps.
-- Written as the board changes, never as a change of their own, so
-- Parun.Engine.Resolve.Common.happenedBetween skips the whole record.
data Snapshots state = MkSnapshots
  { -- | CR 608.2h / 113.7a: last known information, keyed by the id an object
    -- had before it left a zone (CR 400.7). Grows for the whole game.
    lastKnown :: !(Map.Map ObjectId.ObjectId LastKnown.LastKnown),
    -- | CR 608.2h for a STACK object, keyed by the id it had before it left the
    -- stack (CR 400.7): the whole object, so an effect armed while it was still
    -- there can put a copy of it onto the stack afterwards (CR 707.10), or cast
    -- one from exile (rule 702.192a's paradigm).
    --
    -- The whole object and not a LastKnown record, because CR 707.10 copies the
    -- DECISIONS -- the modes, the targets, the value of X, the announced costs --
    -- and CR 109.3 makes none of them a characteristic, so `lastKnown` has
    -- nowhere to keep them.
    --
    -- THREE writers, and the ARMING one wins: an effect that arms a copier files
    -- the object as that effect copies it -- rule 702.50a's epic files the spell
    -- without its own ability ("copy this spell except for its epic ability")
    -- rather than making Effect.CopyStackObject case on which keyword armed it
    -- (Parun.Engine.Resolve.applyEpic) -- and the zone-change funnel files
    -- whatever is left unfiled as the object leaves the stack
    -- (Parun.Engine.Event.changeZoneAttaching), which is CR 608.2h for a trigger
    -- whose spell was countered out from under it -- and Parun.Engine.Game.cease
    -- files an ability as it ceases, the same reading for "copy that ability".
    -- Grows for the whole game, as `lastKnown` does.
    stackArchive :: !(Map.Map ObjectId.ObjectId Object.Object),
    -- | CR 603.10: the battlefield as it stood immediately after each unscanned
    -- event, keyed by group, one lazy entry per group; read by
    -- Event.eventTriggers, cleared as the scan consumes the log.
    battlefieldWhenTriggered :: !(Map.Map EventGroup.EventGroup (Map.Map ObjectId.ObjectId (BattlefieldCandidate.BattlefieldCandidate ProjectedCharacteristics.ProjectedCharacteristics))),
    -- | CR 603.4: the whole game as it stood immediately after each unscanned
    -- event, keyed by group, which an intervening "if" is checked against; each
    -- entry's own copy of this field is empty. Written by
    -- Parun.Engine.Event.Trigger.fileBoards, cleared with `battlefieldWhenTriggered`.
    boardAfterEvent :: !(Map.Map EventGroup.EventGroup state),
    -- | CR 603.10a / 603.10b / 603.10c / 603.10d / 603.10f: the whole game
    -- immediately before each unscanned event that took a permanent off the
    -- battlefield, phased one out, unattached one, changed a permanent's
    -- controller or saw a player leave, keyed by group, the board a look-back
    -- ability reads; each entry's own copy of this field, of `boardAfterEvent`
    -- and of `boardAtControlSample` is empty. Written by
    -- Parun.Engine.Event.fileBoardBefore, for Parun.Engine.Event.sampleControl,
    -- Parun.Engine.Event.unattach, Parun.Engine.Phasing.phaseOutSet and
    -- Parun.Engine.Departure.leaveAll too, cleared with
    -- `battlefieldWhenTriggered`.
    boardBeforeEvent :: !(Map.Map EventGroup.EventGroup state),
    -- | Who controlled each permanent the last time
    -- Parun.Engine.Event.sampleControl looked; diffing it against the live
    -- projection is what yields GameEvent.ControlChanged (CR 603.2). Rebuilt
    -- at every sample, so a first sighting is not a change.
    controlSample :: !(Map.Map ObjectId.ObjectId PlayerId.PlayerId),
    -- | The whole game as `controlSample` was taken from it, the board a
    -- control change the next sample finds is filed as having followed
    -- (`boardBeforeEvent`). Its own copy of this field and of the filed boards is
    -- empty.
    boardAtControlSample :: !(Maybe state),
    -- | CR 603.8 across CR 725.1c's or 725.2c's check: the board before each
    -- pass of it that performed a state-based action, oldest first, which the
    -- next settle opens with (Parun.Engine.Engine.performSettle). Each entry's
    -- own copy of this field is empty.
    heldBoards :: ![state]
  }
  deriving (Eq, Ord, Show)
