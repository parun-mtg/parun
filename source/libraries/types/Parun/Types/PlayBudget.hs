module Parun.Types.PlayBudget where

import qualified Data.Set as Set
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayAllowance as PlayAllowance
import qualified Parun.Types.Timestamp as Timestamp

-- | CR 601.3: how many more of the cards one Effect.GrantPlayFromExile covers
-- may be played under it -- March of Reckless Joy's "you may play up to two of
-- those cards", Aminatou's Augury's one of each nonland card type. Each covered card's Parun.Types.ExilePlayPermission holds the
-- same value, and Parun.Engine.Cast.spendPlayBudget spends it across all of
-- them.
data PlayBudget = MkPlayBudget
  { -- | The plays left. Spending the last clears every permission sharing
    -- this budget.
    remaining :: PlayAllowance.PlayAllowance,
    -- | The objects the grant covered as it was granted, which is which
    -- permissions share this budget. An id that has since left exile names
    -- nothing (CR 400.7).
    covers :: Set.Set ObjectId.ObjectId,
    -- | When the grant was made, which tells this budget from another grant's
    -- over the same cards (#5294).
    granted :: Timestamp.Timestamp
  }
  deriving (Eq, Ord, Show)
