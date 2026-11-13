module Parun.Types.Cloak where

import qualified Parun.Types.PlayerRef as PlayerRef

-- | CR 701.58a: who cloaks the top card of whose library.
data Cloak = MkCloak
  { -- | The players instructed to cloak, under whose control the permanent
    -- enters (CR 110.2a).
    player :: PlayerRef.PlayerRef,
    -- | Whose library the top card comes off: Nothing is each instructed
    -- player's own (Ransom Note), Just another player's (Etrata, Deadly
    -- Fugitive's "that player's library").
    library :: Maybe PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
