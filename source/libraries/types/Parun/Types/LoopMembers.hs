module Parun.Types.LoopMembers where

-- | Which of the players and objects a CR 608.2f loop sweeps it actually runs
-- over (Parun.Types.ForEach).
data LoopMembers
  = -- | Every swept member, CR 608.2f's "for each ...".
    Every
  | -- | Any number of them, chosen once by the resolving controller before the
    -- first iteration (CR 608.2d) -- rule 702.116a's "for each opponent ..., you
    -- may".
    AnyNumber
  | -- | Exactly one of them, chosen likewise (CR 608.2d) -- Zevlor, Elturel
    -- Exile's "choose that player or a permanent they control".
    One
  deriving (Bounded, Enum, Eq, Ord, Show)
