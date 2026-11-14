module Parun.Types.DamageTrigger where

import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamageRecipient as DamageRecipient

-- | CR 603.2 over CR 120.1's damage: whose damage, of which kind, and to which
-- recipients, and how much of it.
data DamageTrigger = MkDamageTrigger
  { actor :: ActingPermanent.ActingPermanent,
    -- | Nothing for damage of any kind.
    kind :: Maybe DamageKind.DamageKind,
    -- | The recipients admitted, any one sufficing; empty for "deals damage"
    -- to anything, which is also what makes one damage event one occurrence
    -- per damager (CR 603.2c).
    recipients :: Set.Set DamageRecipient.DamageRecipient,
    -- | The least damage the trigger event deals, summed over a batch the
    -- recipients leave whole (CR 603.2c); 0 for any amount (Spinneret and
    -- Spiderling's "deals 4 or more damage").
    atLeast :: Natural
  }
  deriving (Eq, Ord, Show)
