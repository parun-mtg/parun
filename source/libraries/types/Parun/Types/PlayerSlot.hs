module Parun.Types.PlayerSlot where

import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

-- | CR 601.2c / 115.10a: the players a relation names and the slot an
-- announcement binds one of them under, for its pool's ZoneScope and its
-- Filter ("from that player's graveyard") -- each in turn
-- (TargetSlot.perPlayer, Riptide Gearhulk's "for each opponent") or the one
-- the activator chooses (ActivatedAbility.chosenPlayer, Lodestone Bauble's
-- "from a player's graveyard").
data PlayerSlot = MkPlayerSlot
  { players :: PlayerRelation.PlayerRelation,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
