module Parun.Types.PayerGrouping where

-- | How the players a CR 118.12 offer names pay it: each their own offer, or
-- one cost between them.
data PayerGrouping
  = -- | CR 118.12a / 101.4: one offer to each player named, in APNAP order.
    Severally
  | -- | CR 805.2: one offer to the named players as a team, paid from what any
    -- of them controls (My Forces Are Innumerable's ruling).
    Jointly
  deriving (Bounded, Enum, Eq, Ord, Show)
