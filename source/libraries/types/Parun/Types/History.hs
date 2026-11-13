module Parun.Types.History where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.AbilityTriggered as AbilityTriggered
import qualified Parun.Types.AttackerDeclared as AttackerDeclared
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.PastTurn as PastTurn
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.SpellWasCast as SpellWasCast

-- | CR 608.2i: what this game has done that a reader asks after the turn's log
-- (TurnRegisters.events) that recorded it is cleared, held whole as
-- GameState.history. Belongs to this game alone, so Parun.Engine.Setup's
-- restartGame (CR 728.1) and subgameStateFrom (CR 730.2) begin it again.
data History = MkHistory
  { -- | CR 502.2 / 732.2: how many spells the previous turn's active player
    -- cast that turn, snapshotted at handoff from the outgoing log, which is
    -- cleared there. Under the shared team turns option, the most any one
    -- player on the previous active team cast (CR 732.2a).
    spellsCastLastTurn :: !Natural.Natural,
    -- | CR 601.2i / 608.2i: every spell cast this game BEFORE the turn in
    -- progress, oldest first, appended at the turn handoff from the log it
    -- clears. Parun.Engine.Count's CastWindow.ThisGame fold joins it to the
    -- live log.
    castsBeforeThisTurn :: !(Seq.Seq SpellWasCast.SpellWasCast),
    -- | CR 120.1 / 608.2i: every damage event dealt this game BEFORE the turn
    -- in progress, oldest first, appended at the turn handoff from the log it
    -- clears, as castsBeforeThisTurn is. Lookback.AsThisObject's
    -- Filter.DealtDamageTo joins it to the live log.
    damageBeforeThisTurn :: !(Seq.Seq DamageEvent.DamageEvent),
    -- | CR 608.2i: every finished turn of this game, oldest first, appended at
    -- the turn handoff (Engine.closeTurn) from the log it clears. The newest is
    -- "last turn" (Game.turnLog), read by the per-seat view and by every
    -- WhichTurn.LastTurn tally.
    pastTurns :: !(Seq.Seq PastTurn.PastTurn),
    -- | CR 508.1a / 608.2i: the attackers declared during each player's OWN
    -- most recent turn, keyed by that player and replaced as their next turn
    -- ends (Engine.beginTurn). A departed player's row stands until their next
    -- turn would have begun (CR 800.4i), where Engine's seat walk removes it.
    attacksInOwnLastTurn :: !(Map.Map PlayerId.PlayerId (Seq.Seq AttackerDeclared.AttackerDeclared)),
    -- | The printed rider "This ability triggers only once"
    -- (Parun.Types.TriggerLimit's OncePerGame), spent here: every triggering
    -- of an ability carrying that rider, as the same record CR 603.3b's log
    -- carries. Stored beside the log rather than read off it, because
    -- `events` is cleared at the turn handoff and this record must outlive
    -- it; a sequence rather than a set, because
    -- Parun.Engine.Event.withinTriggerLimit COUNTS these records exactly as it
    -- counts the log's (CR 113.2c).
    triggeredThisGame :: !(Seq.Seq AbilityTriggered.AbilityTriggered),
    -- | CR 702.192a: the names of the spells each player controlled that have
    -- resolved this game, as they resolved, written by
    -- Parun.Engine.Resolve.noteResolved.
    resolvedNames :: !(Map.Map PlayerId.PlayerId (Set.Set CardName.CardName))
  }
  deriving (Eq, Ord, Show)
