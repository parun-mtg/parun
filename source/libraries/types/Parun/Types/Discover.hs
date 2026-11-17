module Parun.Types.Discover where

import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's Discover arm: the N, and where the
-- discovered card is bound.
--
-- The `slot` is CR 701.57c's "discovered card", the final card exiled when its
-- mana value is N or less: Hit the Mother Lode's "the discovered card's mana
-- value". The id bound is the one it had in exile, which it has left by the
-- time a later clause reads it, so that clause reads it by last known
-- information (CR 608.2h). Nothing is bound when the walk ran out of library.
-- Absent for every discover nothing looks back at.
data Discover = MkDiscover
  { quantity :: Quantity.Quantity,
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
