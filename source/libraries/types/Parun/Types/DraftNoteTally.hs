module Parun.Types.DraftNoteTally where

import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.NoteFold as NoteFold
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Quantity's DraftNotes arm: the numbers a player
-- recorded during the draft for cards of this name (CR 905.2b,
-- Player.draftNotes), folded into one. A LEAF, for
-- Parun.Types.CompletedDungeon's reason.
data DraftNoteTally = MkDraftNoteTally
  { player :: PlayerRef.PlayerRef,
    card :: CardName.CardName,
    fold :: NoteFold.NoteFold
  }
  deriving (Eq, Ord, Show)
