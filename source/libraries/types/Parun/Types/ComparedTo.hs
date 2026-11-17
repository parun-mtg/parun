module Parun.Types.ComparedTo where

import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Quantity as Quantity

-- | One comparison whose measured side is the EVENT's own amount, so only the
-- threshold is written: Parun.Types.DamagePattern's `whatAmount`, Ojer Axonil,
-- Deepest Might's "an amount of noncombat damage less than Ojer Axonil's
-- power". Parun.Types.Compares with the measured Quantity taken from the event
-- rather than authored.
data ComparedTo = MkComparedTo
  { comparison :: Comparison.Comparison,
    threshold :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
