module Parun.Types.UpToCasts where

import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TotalCap as TotalCap

-- | Parun.Types.CastRepetition.UpTo's bounds: how many casts, and what their
-- weights may sum to (CR 608.2d).
data UpToCasts = MkUpToCasts
  { -- | Talent of the Telepath's "up to two", read once before the first cast
    -- (CR 608.2h).
    casts :: Quantity.Quantity,
    -- | Invoke Calamity's "with total mana value 6 or less", read the same way;
    -- Nothing caps only the count.
    total :: Maybe TotalCap.TotalCap
  }
  deriving (Eq, Ord, Show)
