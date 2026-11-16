module Parun.Types.Cloak where

import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

-- | CR 701.58a: who cloaks the top card of whose library.
data Cloak = MkCloak
  { -- | The players instructed to cloak, under whose control the permanent
    -- enters (CR 110.2a).
    player :: PlayerRef.PlayerRef,
    -- | Whose library the top card comes off: Nothing is each instructed
    -- player's own (Ransom Note), Just another player's (Etrata, Deadly
    -- Fugitive's "that player's library").
    library :: Maybe PlayerRef.PlayerRef,
    -- | The permanents the cloaks put onto the battlefield, for a later effect
    -- of the same resolution to name (Cryptic Coat's "attach this Equipment to
    -- it"). Absent for every cloak nothing looks back at.
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
