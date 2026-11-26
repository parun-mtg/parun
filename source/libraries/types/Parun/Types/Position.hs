module Parun.Types.Position where

import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Parun.Types.Features as Features
import qualified Parun.Types.PlayerId as PlayerId

-- | A position a player made a decision at priority in: the turn's number and
-- every player's generic features, the rows `parun fit` fits the search bot's
-- weights to (Parun.Play.Fit).
data Position = MkPosition
  { turn :: Natural,
    features :: Map.Map PlayerId.PlayerId Features.Features
  }
  deriving (Eq, Show)
