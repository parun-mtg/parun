module Parun.Types.Format where

-- | CR 100.2: the construction rules a deck is built under. A format's card
-- pool is not represented (#6395).
data Format
  = -- | CR 100.2a / 100.4a: each player builds their own deck ahead of time.
    Constructed
  | -- | CR 100.2b: each player builds from the same quantity of unopened product.
    Limited
  deriving (Bounded, Enum, Eq, Ord, Show)
