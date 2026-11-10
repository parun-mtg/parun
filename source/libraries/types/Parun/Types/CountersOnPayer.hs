module Parun.Types.CountersOnPayer where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind

-- | The payload of Parun.Types.CostComponent's PutCountersOnPayer arm: this many
-- counters of one kind, given to the paying player (CR 118.1, CR 122.1). The
-- Serpent Society's "Ward--Get five poison counters" is the printing.
--
-- Parun.Types.CountersOnThis' shape on CR 122.1's other holder, kept apart
-- because Parun.Types.PlayerCounterKind is a disjoint domain.
data CountersOnPayer = MkCountersOnPayer
  { kind :: PlayerCounterKind.PlayerCounterKind,
    count :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
