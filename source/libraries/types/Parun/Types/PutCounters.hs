module Parun.Types.PutCounters where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's PutCounters arm (#1305). The
-- `quantity` is read per permanent, so a division's part (Quantity.Share) is
-- each one's own: Crashing Wave's "distribute three stun counters" is an
-- Effect.Divide and then this (CR 608.2d).
--
-- An ObjectRef rather than a bare slot, so that Renegade Krasis' "each other
-- creature you control with a +1/+1 counter on it" can be written: CR 115.10a
-- makes such a set a description and never a target, which is exactly the
-- distinction that type draws.
--
-- ONE kind per instruction, which is all CR 701.10e's sentence asks for and what
-- Aragorn, Hornburg Hero writes. A card's "each kind of counter" (Vorel of the
-- Hull Clade) is Parun.Types.PutCountersFrom from a permanent onto itself.
--
-- Each named permanent gets its OWN call to Event.putCounters, because CR 614.16
-- replaces one placement at a time: a Hardened Scales seeing three creatures gets
-- three opportunities, not one.
--
-- TOTALATMOST is Clockwork Beast's "this ability can't cause the total number of
-- +1/+0 counters on this creature to be greater than seven", asked of the settled
-- placement after CR 616.1's loop, so a Doubling Season can't carry it past the
-- limit (CR 101.2, CR 609.3). The scenario clockwork-beast-stops-at-seven-under-doubling-season
-- proves it.
--
-- `slot` is where the permanents this instruction ACTUALLY put a counter on are
-- bound as a group, for a later effect of the same resolution to count or name
-- -- Call the Spirit Dragons' "if you put +1/+1 counters on five Dragons this
-- way". Destroy's field, for Destroy's reason: it is the funnel's answer, so a
-- placement CR 614 replaced down to none (Vorinclex, Monstrous Raider) or CR
-- 101.2 forbade (Solemnity) does not count. Nothing is bound when nothing was
-- placed; absent for a placement that is never looked back at.
data PutCounters = MkPutCounters
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    quantity :: Quantity.Quantity,
    ref :: ObjectRef.ObjectRef,
    totalAtMost :: Maybe Natural.Natural,
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
