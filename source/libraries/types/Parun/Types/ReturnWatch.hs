module Parun.Types.ReturnWatch where

import qualified Parun.Types.EntryLink as EntryLink
import qualified Parun.Types.ReturnEnding as ReturnEnding
import qualified Parun.Types.Zone as Zone

-- | CR 610.3: one object moved "until" an event, and what the game must
-- remember to move it back. Keyed by the incarnation the move minted (CR 400.7).
--
-- Board state rather than a field on the moved Object: it is a relation that
-- outlives the move's source, and CR 400.7 would strip it from an object that
-- moved again anyway.
data ReturnWatch = MkReturnWatch
  { -- | The specified event whose happening ends the duration.
    ending :: ReturnEnding.ReturnEnding,
    -- | CR 610.3's "previous zone": where the second one-shot effect puts the
    -- object back. Recorded at the move rather than assumed, since the rule
    -- names the zone the object came from.
    zone :: Zone.Zone,
    -- | CR 610.3 / 607.5: the object whose effect made the move, and with
    -- which acquisition of its abilities, linked to what the second one-shot
    -- effect puts onto the battlefield (Filter.EnteredWithSource): Lagrella,
    -- the Magpie's "an exiled card enters ... this way".
    link :: EntryLink.EntryLink
  }
  deriving (Eq, Ord, Show)
