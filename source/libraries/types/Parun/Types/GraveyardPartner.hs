module Parun.Types.GraveyardPartner where

-- | The other half of a Parun.Types.Pool.CardsInGraveyardAnd slot: what one
-- target ranges over beside the cards in a graveyard (CR 115.2).
data GraveyardPartner
  = -- | CR 109.2: creatures on the battlefield (ToCreature) -- Savior of
    -- Ollenbock's "creature from the battlefield or creature card from a graveyard".
    BattlefieldCreatures
  | -- | CR 406.1: the cards in exile (ToObject) -- Sorceress's Schemes's "card
    -- from your graveyard or exiled card with flashback you own".
    ExiledCards
  deriving (Bounded, Enum, Eq, Ord, Show)
