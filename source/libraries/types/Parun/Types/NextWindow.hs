module Parun.Types.NextWindow where

-- | CR 611.2a: the unit of a "during ... next" window, one of a named player's.
data NextWindow
  = -- | "during that player's next turn" (Wall of Dust).
    Turn
  | -- | CR 502.3: "during their next untap step" (Exhaustion).
    UntapStep
  | -- | CR 506.1: "during its controller's next combat phase" (Sizzling Soloist).
    CombatPhase
  deriving (Bounded, Enum, Eq, Ord, Show)
