module Parun.Types.Again where

import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.Quantity as Quantity

-- | What decides whether a Parun.Types.Repeat's process runs again.
data Again
  = -- | CR 608.2d: that player is asked after each run whether to go again.
    AskPlayer PlayerRef.PlayerRef
  | -- | CR 608.2c: that many more runs, read once before the first; nobody is
    -- asked.
    MoreTimes Quantity.Quantity
  deriving (Eq, Ord, Show)
