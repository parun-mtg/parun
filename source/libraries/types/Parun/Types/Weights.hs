module Parun.Types.Weights where

import qualified Parun.Types.Features as Features

-- | What the search bot (Parun.Play.Search) values in a position it cuts a
-- playout off at: its own features weighted by `ours`, less the strongest
-- opponent's weighted by `theirs`. Read from a file (Parun.Weights) for a
-- persona or a tuning run.
data Weights = MkWeights
  { ours :: Features.Features,
    theirs :: Features.Features
  }
  deriving (Eq, Show)

-- | Every feature weighted 1 on each side: the weights unless told otherwise.
neutral :: Weights
neutral = let one = Features.MkFeatures 1 1 1 1 1 1 in MkWeights one one
