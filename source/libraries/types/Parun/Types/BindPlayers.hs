module Parun.Types.BindPlayers where

import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's BindPlayers arm: the players, and the
-- slot they are bound under.
data BindPlayers = MkBindPlayers
  { players :: PlayerRef.PlayerRef,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
