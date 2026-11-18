module Parun.Types.NameReading where

-- | Which names of a card in the Oracle card reference a name question counts,
-- and the characteristics each is judged by (Parun.Types.Prompt's
-- ReferenceNames).
data NameReading
  = -- | The names the card has where it is, off the stack, plus a double-faced
    -- card's back face: a name grant's (CR 612.7).
    OffStack
  | -- | Every name a player may choose, each judged by its own face (CR 201.4).
    Choosable
  deriving (Bounded, Enum, Eq, Ord, Show)
