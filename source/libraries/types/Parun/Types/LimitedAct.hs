module Parun.Types.LimitedAct where

-- | Which act Parun.Types.ForbiddenAct's MoreThanEachTurn counts against its
-- limit, each read off its own register of the turn by
-- Parun.Engine.PlayerEffect.prohibitsMore.
data LimitedAct
  = -- | CR 121.2b: drawing a card.
    Draw
  | -- | CR 701.49: venturing into the dungeon, CR 701.49d's variants included.
    Venture
  deriving (Bounded, Enum, Eq, Ord, Show)
