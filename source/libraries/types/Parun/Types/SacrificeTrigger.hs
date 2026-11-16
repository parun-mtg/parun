module Parun.Types.SacrificeTrigger where

import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.PlayerActsOn as PlayerActsOn

-- | CR 603.2 over CR 701.21a's sacrifice: whose, of what, and per permanent or
-- per sacrifice event (CR 603.2c).
data SacrificeTrigger = MkSacrificeTrigger
  { acts :: PlayerActsOn.PlayerActsOn,
    batching :: Batching.Batching
  }
  deriving (Eq, Ord, Show)
