module Parun.Types.GraveyardCostDestination where

-- | Where Parun.Types.MoveCardsFromGraveyard puts the cards it moves.
data GraveyardCostDestination
  = -- | CR 406.2 / Headless Skaab: exile them.
    Exile
  | -- | CR 400.3 / Gurzigost: put them on the bottom of their owner's library.
    LibraryBottom
  deriving (Bounded, Enum, Eq, Ord, Show)
