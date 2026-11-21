module Parun.Types.Copies where

-- | Which of CR 601.2i's cast and CR 707.10's copy a cast trigger watches
-- (Parun.Types.SpellCast.copies). A copy is not cast (CR 707.10); a cast copy
-- (CR 707.12) is a cast, never a copy.
data Copies
  = -- | "Whenever ... casts": casts only.
    NoCopies
  | -- | Magecraft's "whenever you cast or copy" (CR 207.2c): casts and copies.
    AlsoCopies
  | -- | Curse of Shaken Faith's "or copies a spell", the copy half of a pair: copies only.
    OnlyCopies
  deriving (Bounded, Enum, Eq, Ord, Show)
