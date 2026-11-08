module Parun.Types.ThrowResult where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CoinFace as CoinFace

-- | What one thrown coin or die came up: the result an offer to throw it again
-- is put over (Goblin Bookie's "reflip any coin or reroll any die").
data ThrowResult
  = -- | CR 705.1: the face a coin landed on.
    CoinCameUp CoinFace.CoinFace
  | -- | CR 706.2b: the natural result a die came up.
    DieCameUp Natural.Natural
  deriving (Eq, Ord, Show)
