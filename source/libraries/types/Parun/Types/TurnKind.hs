module Parun.Types.TurnKind where

-- | CR 500.7 / 614.10: which turns a whole-turn skip names, or which kind of
-- turn Parun.Engine.Engine offers up as one would begin.
data TurnKind
  = -- | Any turn (CR 500.11); offered for a turn no effect created.
    AnyTurn
  | -- | A turn a spell or ability created (CR 500.7).
    ExtraTurn
  deriving (Bounded, Enum, Eq, Ord, Show)
