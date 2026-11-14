module Parun.Types.EntriesTrigger where

import qualified Parun.Types.EntryTriggering as EntryTriggering
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.TriggeringMove as TriggeringMove

-- | A rule over the abilities a permanent entering or dying causes to trigger,
-- read for the player whose abilities they are
-- (Parun.Types.PlayerEffect.EntriesTrigger).
data EntriesTrigger = MkEntriesTrigger
  { -- | Which permanents it applies to: an entrant read as it exists on the
    -- battlefield, a dying one as it last existed there.
    entrants :: Filter.Filter Keyword.Keyword,
    move :: TriggeringMove.TriggeringMove,
    triggering :: EntryTriggering.EntryTriggering
  }
  deriving (Eq, Ord, Show)
