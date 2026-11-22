module Parun.Types.Polarity where

-- | Which way an effect bears on what a target slot holds, as the playout
-- policy reads it (Parun.Play.Polarity.polarity).
data Polarity
  = -- | It costs the target's side: damage, destruction, a loss.
    Harms
  | -- | It gains the target's side: a pump, a card, life.
    Helps
  deriving (Bounded, Enum, Eq, Ord, Show)
