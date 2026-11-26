module Parun.Types.PaidResource where

-- | What an Effect.PayAny's "pay any amount of" spends.
data PaidResource
  = -- | CR 107.14: energy counters (Harnessed Lightning).
    Energy
  | -- | CR 119.4: life (Maralen of the Mornsong Avatar).
    Life
  deriving (Bounded, Enum, Eq, Ord, Show)
