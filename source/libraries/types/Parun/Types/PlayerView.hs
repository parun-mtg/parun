module Parun.Types.PlayerView where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.Daytime as Daytime
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.Undoing as Undoing

-- | CR 400.2 / 724.4: everything one seat may see of a game, and nothing else.
data PlayerView = MkPlayerView
  { viewer :: PlayerId.PlayerId,
    -- | CR 728.1 / 730.1a: which game of the match this is
    -- (MatchScope.gameNumber), whose turns `turn` counts.
    game :: Natural.Natural,
    turn :: Natural.Natural,
    phase :: Phase.Phase,
    active :: PlayerId.PlayerId,
    priority :: Maybe PlayerId.PlayerId,
    seats :: [SeatView.SeatView],
    battlefield :: [ObjectView.ObjectView],
    -- | CR 702.26b: the phased-out permanents, which every seat sees though
    -- the game treats them as though they don't exist.
    phasedOut :: [ObjectView.ObjectView],
    -- | CR 408: what is face up in the command zone: emblems, dungeons,
    -- commanders, planes and schemes.
    command :: [ObjectView.ObjectView],
    -- | Top first, as TurnRegisters.stack.
    stack :: [ObjectView.ObjectView],
    -- | CR 508.1b: each attacking creature and what it attacks, as Combat.attackers.
    attackers :: Map.Map ObjectId.ObjectId AttackTarget.AttackTarget,
    -- | CR 509.1a: each blocked attacker and the creatures blocking it, as Combat.blockers.
    blockers :: Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId),
    -- | CR 807.5: each parked Grand Melee turn's stack, top first, by its
    -- marker's number (GameState.parkedTurns).
    parkedStacks :: Map.Map Natural.Natural [ObjectView.ObjectView],
    -- | This turn's log entries the seat witnessed (CR 400.2, 701.20a, 701.20e).
    witnessed :: [LoggedEvent.LoggedEvent],
    -- | The previous turn's log entries the seat witnessed, for a client that
    -- last looked before that turn ended (CR 514.1's discard, for one).
    witnessedLastTurn :: [LoggedEvent.LoggedEvent],
    -- | CR 726.1: the monarch.
    monarch :: Maybe PlayerId.PlayerId,
    -- | CR 727.1: who has the initiative.
    initiative :: Maybe PlayerId.PlayerId,
    -- | CR 732.1: day or night, Nothing until it first becomes either.
    daytime :: Maybe Daytime.Daytime,
    result :: Maybe Result.Result,
    -- | CR 734.1: why the seat's last cast, activation or combat declaration
    -- was reversed, until it next chooses one (Bookkeeping.undone).
    undone :: Maybe Undoing.Undoing
  }
  deriving (Eq, Show)
