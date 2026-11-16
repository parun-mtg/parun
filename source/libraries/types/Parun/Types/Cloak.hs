module Parun.Types.Cloak where

import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

-- | CR 701.58a: who cloaks which cards.
data Cloak = MkCloak
  { -- | The players instructed to cloak, under whose control the permanent
    -- enters (CR 110.2a).
    player :: PlayerRef.PlayerRef,
    -- | The cards cloaked: Nothing is the top card of each instructed player's
    -- own library (Ransom Note), Just the objects the ref names, asked once
    -- before any is cloaked (Etrata, Deadly Fugitive's "that player's library",
    -- Vannifar, Evolved Enigma's "a card from your hand").
    cards :: Maybe ObjectRef.ObjectRef,
    -- | Whether those cards, cloaked one at a time (CR 701.58e), go in a
    -- random order: Expose the Culprit's "shuffle that pile, then cloak them".
    shuffled :: Bool,
    -- | The permanents the cloaks put onto the battlefield, for a later effect
    -- of the same resolution to name (Cryptic Coat's "attach this Equipment to
    -- it"). Absent for every cloak nothing looks back at.
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
