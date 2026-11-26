module Parun.Types.PayAny where

import qualified Parun.Types.PaidResource as PaidResource
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's PayAny arm: what is paid, and where
-- the amount goes.
data PayAny = MkPayAny
  { resource :: PaidResource.PaidResource,
    -- | Where the amount is bound for a later effect of the same resolution
    -- to read.
    slot :: SlotName.SlotName,
    -- | CR 607.2d: whether another ability of the source refers to the amount
    -- (Maralen of the Mornsong Avatar's "the amount of life paid with"), which
    -- is then written to Object.chosenNumber for Quantity.ChosenNumber to read.
    linked :: Bool
  }
  deriving (Eq, Ord, Show)
