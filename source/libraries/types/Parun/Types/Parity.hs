module Parun.Types.Parity where

-- | Whether a whole number is divisible by two, zero being even: the "odd or
-- even" Gollum, Riddle Master's controller chooses, whose reminder text and
-- Void Winnower's settle zero.
data Parity
  = -- | Divisible by two, zero included.
    Even
  | -- | Not divisible by two.
    Odd
  deriving (Bounded, Enum, Eq, Ord, Show)
