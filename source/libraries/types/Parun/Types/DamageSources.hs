module Parun.Types.DamageSources where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.SourceDealtDamage as SourceDealtDamage

-- | CR 120.1 / 608.2i: which of this turn's damage events
-- Parun.Types.ObjectRef's DamageSourcesThisTurn takes the sources of.
data DamageSources = MkDamageSources
  { -- | Which damage counts: its kind and its recipients.
    dealt :: SourceDealtDamage.SourceDealtDamage Keyword.Keyword,
    -- | The source as it dealt the damage (CR 608.2h), read off the event's
    -- deal-time riders -- Maelstrom Archangel Avatar's "creature you
    -- controlled at the time it dealt combat damage".
    by :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
