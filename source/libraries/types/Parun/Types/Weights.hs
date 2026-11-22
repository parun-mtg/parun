module Parun.Types.Weights where

import qualified Parun.Types.Features as Features

-- | What the search bot (Parun.Play.Search) values in a position it cuts a
-- playout off at: its own features weighted by `ours`, less the strongest
-- opponent's weighted by `theirs`. Loaded as data (data/weights).
data Weights = MkWeights
  { ours :: Features.Features,
    theirs :: Features.Features
  }
  deriving (Eq, Show)
