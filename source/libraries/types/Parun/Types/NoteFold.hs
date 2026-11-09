module Parun.Types.NoteFold where

-- | How Parun.Types.DraftNoteTally folds the numbers a player recorded during
-- the draft (CR 905.2b) for cards of one name into one.
data NoteFold
  = -- | Their sum -- Cogwork Grinder's "the number of cards you removed from
    -- the draft with cards named Cogwork Grinder", each copy's removals one
    -- number.
    Total
  | -- | The greatest of them -- Lurking Automaton's "the highest number you
    -- noted for cards named Lurking Automaton".
    Highest
  deriving (Bounded, Enum, Eq, Ord, Show)
