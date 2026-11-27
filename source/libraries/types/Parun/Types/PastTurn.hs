module Parun.Types.PastTurn where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Numeric.Natural as Natural
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 608.2i: one finished turn, as History.pastTurns keeps it once the
-- handoff (Engine.closeTurn) clears the log that recorded it.
data PastTurn = MkPastTurn
  { -- | CR 102.1 / 805.4: whose turn it was, in Turn.activePlayers' order.
    activePlayers :: ![PlayerId.PlayerId],
    -- | CR 608.2i: the turn's whole log (TurnRegisters.events), oldest first.
    events :: !(Seq.Seq LoggedEvent.LoggedEvent),
    -- | CR 121.1: how many cards each player drew during the turn
    -- (TurnRegisters.drawsThisTurn), CR 103.5's opening hands not among them,
    -- though the log holds their GameEvent.Drew.
    draws :: !(Map.Map PlayerId.PlayerId Natural.Natural)
  }
  deriving (Eq, Ord, Show)
