module Parun.Types.LostPermanent where

import qualified Parun.Types.PlayerActsOn as PlayerActsOn
import qualified Parun.Types.SlotName as SlotName

-- | CR 603.10d / 603.7: which permanent a lose-control trigger watches change
-- hands away from "you".
data LostPermanent
  = -- | CR 603.10d: the bearer itself (Khârn, the Betrayer).
    Self
  | -- | CR 603.7c: the object bound at this slot as the ability was created (Ray
    -- of Command).
    Bound SlotName.SlotName
  | -- | CR 603.10d: any permanent matching the filter, taken by a player the
    -- relation admits (Zidane, Tantalus Thief).
    Matching PlayerActsOn.PlayerActsOn
  deriving (Eq, Ord, Show)
