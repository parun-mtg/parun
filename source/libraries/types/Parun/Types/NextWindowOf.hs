module Parun.Types.NextWindowOf where

import qualified Parun.Types.NextWindow as NextWindow
import qualified Parun.Types.PlayerRef as PlayerRef

-- | CR 611.2a: the payload of Parun.Types.Duration's DuringNextOf arm: which
-- window, of whose turn.
data NextWindowOf = MkNextWindowOf
  { window :: NextWindow.NextWindow,
    player :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
