module Parun.Types.EachKind where

import qualified Parun.Types.KindStep as KindStep
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.AnotherCounter's EachKind arm: every kind of
-- counter on each target in a slot, and what each kind gets (CR 122.1).
data EachKind = MkEachKind
  { -- | The slot the targets are read from.
    slot :: SlotName.SlotName,
    step :: KindStep.KindStep
  }
  deriving (Eq, Ord, Show)
