module Parun.Types.AnotherCounter where

import qualified Parun.Types.SlotName as SlotName

-- | Which kinds the targets in a slot each get one more counter of, for
-- Parun.Types.Effect's targeted Proliferate (CR 122.1, CR 115.1).
data AnotherCounter
  = -- | Powerful Broker's "for each kind of counter on target permanent or player": one of every kind there (CR 122.1).
    EachKind SlotName.SlotName
  | -- | Dathon, His Heart Open's "choose a counter on target enchantment": one of a kind its controller picks among those there (CR 122.1).
    ChosenKind SlotName.SlotName
  deriving (Eq, Ord, Show)

-- | The slot the targets are read from.
slotOf :: AnotherCounter -> SlotName.SlotName
slotOf x = case x of
  EachKind slot -> slot
  ChosenKind slot -> slot
