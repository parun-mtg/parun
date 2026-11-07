module Parun.Types.FreeCastFromHand where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Quantity as Quantity

-- | The payload of Parun.Types.PlayerEffect's CastFromHandWithoutPayingManaCost
-- arm: which spells CR 118.9's standing grant covers, and the number their
-- filter's EnclosingAmount operand compares against.
data FreeCastFromHand = MkFreeCastFromHand
  { -- | Read off the card's CR 613.1 view in the hand, as
    -- Parun.Types.CastFromZone's `matching` is.
    spells :: Filter.Filter Keyword.Keyword,
    -- | CR 109.5: read for the grant's SOURCE, as Parun.Types.AffectedUnless's
    -- `amount` is (Omnipresence's "the number of creatures you control").
    -- Nothing where the filter compares against no computed number.
    amount :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
