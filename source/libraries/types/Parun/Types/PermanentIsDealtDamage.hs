module Parun.Types.PermanentIsDealtDamage where

import Numeric.Natural (Natural)
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.DealtDamage as DealtDamage

-- | CR 120.3 / 603.2c: which permanent's damage fires the ability, and whether
-- each source's damage is its own occurrence.
data PermanentIsDealtDamage = MkPermanentIsDealtDamage
  { dealt :: DealtDamage.DealtDamage ActingPermanent.ActingPermanent,
    -- | OneOrMore for "is dealt damage": one occurrence per damaged permanent
    -- per simultaneous batch (Ripjaw Raptor's and Fungusaur's rulings).
    -- EachOccurrence for "a source deals damage to": one per damage event, so
    -- per source (Belltower Sphinx).
    batching :: Batching.Batching,
    -- | CR 120.10: only damage over the permanent's bar, "is dealt excess
    -- damage" (Fall of Cair Andros); its amount is the excess.
    excess :: Bool,
    -- | The least damage the trigger event deals, summed over its batch (CR
    -- 603.2c); 0 for any amount (Innocent Bystander's "is dealt 3 or more
    -- damage").
    atLeast :: Natural
  }
  deriving (Eq, Ord, Show)
