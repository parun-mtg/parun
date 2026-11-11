module Parun.Types.GrantZone where

-- | CR 601.3: where a card Parun.Types.GrantPlayFromExile covers must lie to be
-- granted. A card a CR 614 replacement sent anywhere else gets nothing.
data GrantZone
  = -- | CR 406: "cards exiled this way".
    Exile
  | -- | CR 404: Jace, Telepath Unbound's "from your graveyard".
    Graveyard
  | -- | CR 701.17c: whichever public zone a milled card moved to -- Ark of
    -- Hunger's "mill a card. You may play that card".
    AnyPublic
  deriving (Bounded, Enum, Eq, Ord, Show)
