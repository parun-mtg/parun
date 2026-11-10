module Parun.Types.FreeCast where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Zone as Zone

-- | The payload of Parun.Types.PlayerEffect's CastWithoutPayingManaCost arm:
-- which spells CR 118.9's grant covers, the number their filter's
-- EnclosingAmount operand compares against, and the zone it is confined to.
data FreeCast = MkFreeCast
  { -- | Read off the card's CR 613.1 view where it is cast from, as
    -- Parun.Types.CastFromZone's `matching` is.
    spells :: Filter.Filter Keyword.Keyword,
    -- | CR 109.5: read for the grant's SOURCE, as Parun.Types.AffectedUnless's
    -- `amount` is (Omnipresence's "the number of creatures you control").
    -- Nothing where the filter compares against no computed number.
    amount :: Maybe Quantity.Quantity,
    -- | The grantee's own zone the cast must begin in, Omniscience's "from
    -- your hand"; Nothing where the sentence names none (World War Hulk's
    -- chapter I).
    from :: Maybe Zone.Zone
  }
  deriving (Eq, Ord, Show)
