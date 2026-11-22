module Parun.Types.OrTapped where

import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.EntryPrice as EntryPrice
import qualified Parun.Types.WithCounters as WithCounters

-- | CR 614.1c: "as this enters, you may [price]. If you don't, it enters
-- tapped", the payload of Parun.Types.EntryRewrite's OrTapped arm.
data OrTapped = MkOrTapped
  { price :: EntryPrice.EntryPrice,
    -- | A second way to avoid the tap, asked only once the price is declined:
    -- Fortified Beachhead's "or you control a Soldier". Nothing for a land the
    -- price alone keeps untapped. Elided rather than written null.
    unless :: Maybe Condition.Condition,
    -- | What the price or the waiver BUYS instead of avoiding the tap: Dragon's
    -- Disciple's "If you do or if you control a Dragon, this creature enters
    -- with a +1/+1 counter on it", declining then changing nothing. Nothing for
    -- the tapped land. Elided rather than written null.
    counters :: Maybe WithCounters.WithCounters
  }
  deriving (Eq, Ord, Show)
