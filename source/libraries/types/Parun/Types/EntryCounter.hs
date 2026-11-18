module Parun.Types.EntryCounter where

import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | One kind of counter an effect says an object enters with (CR 122.6), and
-- which entrant gets it: EntryRiders.counters' key.
--
-- `onlyIf` is the entrant the effect conditions the counters on, read against
-- the permanent as it would exist on the battlefield (CR 614.12) -- Chief
-- Montgomery Scott's "X additional +1/+1 counters on it if it's a creature and
-- X additional charge counters on it if it's a Spacecraft". 'Nothing' for every
-- other rider. Part of the KEY rather than the count because the condition
-- picks which kinds the entrant gets, not how many: one kind could carry two
-- gates. Read in the entrant's own context (Projection.sourceContext), so a
-- gate can ask only after the entrant's characteristics.
data EntryCounter = MkEntryCounter
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    onlyIf :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)

-- | Counters of a kind every entrant gets (CR 122.6).
unconditional :: CounterKind.CounterKind Keyword.Keyword -> EntryCounter
unconditional k = MkEntryCounter {kind = k, onlyIf = Nothing}
