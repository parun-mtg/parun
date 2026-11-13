module Parun.Types.EntryWindow where

-- | How far back Parun.Types.Filter's EnteredThisTurn looks for the candidate's
-- arrival (Parun.Types.EnteredZone's `window`).
data EntryWindow
  = -- | CR 608.2i: the turn in progress, TurnRegisters.events.
    ThisTurn
  | -- | CR 608.2i: every turn since the perspective's own most recent turn ended
    -- (Game.ownLastTurn) -- Premature Burial's "entered since your last turn
    -- ended".
    SinceOwnLastTurn
  deriving (Bounded, Enum, Eq, Ord, Show)
