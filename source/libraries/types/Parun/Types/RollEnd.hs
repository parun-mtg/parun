module Parun.Types.RollEnd where

-- | Which end of the used results Parun.Types.RollExtreme names, where several
-- players rolled (CR 706.1).
data RollEnd
  = -- | Every player whose result was the highest, ties included (Chaos
    -- Dragon's "those players").
    Highest
  | -- | The one player whose result was the lowest, the players tied for it
    -- rerolling until one is (Ricochet's "reroll to break ties, if
    -- necessary"; docs/decisions.md, "Who rerolls Ricochet's tie").
    LowestBreakingTies
  deriving (Bounded, Enum, Eq, Ord, Show)
