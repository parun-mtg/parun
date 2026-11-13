module Parun.Types.History where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Types.AbilityTriggered as AbilityTriggered
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.PastTurn as PastTurn
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 608.2i: what this game has done that a reader asks after the turn's log
-- (TurnRegisters.events) that recorded it is cleared, held whole as
-- GameState.history. Belongs to this game alone, so Parun.Engine.Setup's
-- restartGame (CR 728.1) and subgameStateFrom (CR 730.2) begin it again.
data History = MkHistory
  { -- | CR 608.2i: every finished turn of this game, oldest first, appended at
    -- the turn handoff (Engine.closeTurn) from the log it clears. The newest is
    -- "last turn" (Game.turnLog), read by the per-seat view, by every
    -- WhichTurn.LastTurn tally and by CR 502.2's count
    -- (Game.spellsCastLastTurn); all of them hold the game's casts
    -- (Game.castsThisGame) and its earlier damage (Game.damageBeforeThisTurn).
    pastTurns :: !(Seq.Seq PastTurn.PastTurn),
    -- | CR 800.4i: the departed players whose own last turn
    -- (Game.ownLastTurn) no longer answers "during their last turn", because
    -- their next turn would have begun (Engine.expireLastTurn).
    expiredLastTurns :: !(Set.Set PlayerId.PlayerId),
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
