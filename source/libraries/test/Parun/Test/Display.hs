module Parun.Test.Display where

-- | How a run shows its failures and progress: whether standard output is a
-- terminal, to keep a progress line on, and whether the run is under GitHub
-- Actions, to annotate.
data Display = MkDisplay
  { live :: Bool,
    annotated :: Bool
  }
  deriving (Eq, Show)
