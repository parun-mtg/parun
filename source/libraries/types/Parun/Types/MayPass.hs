module Parun.Types.MayPass where

-- | How a "may" asked of several seats is put to them (Parun.Types.Optionality).
data MayPass
  = -- | CR 608.2e: every seat answers, in APNAP order, before the clause runs for the seats that accepted.
    AllSeatsFirst
  | -- | CR 101.4's order, each seat answering and the clause running for it before the next seat is asked.
    SeatBySeat
  deriving (Bounded, Enum, Eq, Ord, Show)
