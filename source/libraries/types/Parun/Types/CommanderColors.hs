module Parun.Types.CommanderColors where

import qualified Data.Set as Set
import qualified Parun.Types.Color as Color
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Quantity's CommanderColors arm (CR 903.4): whose
-- commanders, and which colours to look for in their colour identity.
data CommanderColors = MkCommanderColors
  { player :: PlayerRef.PlayerRef,
    colors :: Set.Set Color.Color
  }
  deriving (Eq, Ord, Show)
