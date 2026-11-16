module Parun.Types.RecipientKind where

-- | CR 120.1: which recipients a damage fold admits.
data RecipientKind
  = -- | CR 120.1: any player or permanent -- Case of the Burning Masks' "sources
    -- you controlled dealt damage".
    AnyRecipient
  | -- | CR 120.3a: a player alone -- Indulge // Excess's "dealt combat damage to a
    -- player".
    PlayerRecipient
  deriving (Bounded, Enum, Eq, Ord, Show)
