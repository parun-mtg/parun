module Parun.Types.CountersOnThis where

import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.CostComponent's RemoveCountersFromThis and
-- PutCountersOnThis arms: this many counters of one kind, taken off or put on
-- the permanent the cost is on (CR 118.1), or the one a slot names. Barkhide
-- Troll's +1\/+1 counter, Hickory Woodlot's depletion counter, Yisan, the
-- Wanderer Bard's verse counter and Warden of the Grove's endure X are
-- printings.
--
-- PARAMETRIC in the keyword for the CounterKind it carries, exactly as
-- Parun.Types.CostComponent is.
data CountersOnThis keyword = MkCountersOnThis
  { kind :: CounterKind.CounterKind keyword,
    -- | CR 107.3a / 107.3i: Krumar Initiate's "endures X" is Announced X, which
    -- Parun.Engine.Cost.substituteX fixes.
    count :: CostAmount.CostAmount,
    -- | CR 701.63a's "that permanent" where it is not the cost's own: Warden of
    -- the Grove's creature that entered. Nothing for the permanent the cost is
    -- on.
    holder :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
