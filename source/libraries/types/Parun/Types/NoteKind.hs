module Parun.Types.NoteKind where

-- | CR 607.2e: what an ability notes for its source.
data NoteKind
  = -- | CR 602.2b: the type and amount of mana spent to activate this ability (Ice Cauldron).
    ManaSpent
  | -- | CR 119.1: the resolving controller's life total (Sigarda's Splendor).
    LifeTotal
  deriving (Bounded, Enum, Eq, Ord, Show)
