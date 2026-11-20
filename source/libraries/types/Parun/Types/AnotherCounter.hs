module Parun.Types.AnotherCounter where

import qualified Parun.Types.EachKind as EachKind
import qualified Parun.Types.SlotName as SlotName

-- | Which kinds the targets in a slot each get one more counter of, for
-- Parun.Types.Effect's targeted Proliferate (CR 122.1, CR 115.1).
data AnotherCounter
  = -- | Powerful Broker's "for each kind of counter on target permanent or player": every kind there, each as the EachKind's step says (CR 122.1).
    EachKind EachKind.EachKind
  | -- | Dathon, His Heart Open's "choose a counter on target enchantment": one of a kind its controller picks among those there (CR 122.1).
    ChosenKind SlotName.SlotName
  deriving (Eq, Ord, Show)

-- | The slot the targets are read from.
slotOf :: AnotherCounter -> SlotName.SlotName
slotOf x = case x of
  EachKind each -> EachKind.slot each
  ChosenKind slot -> slot
