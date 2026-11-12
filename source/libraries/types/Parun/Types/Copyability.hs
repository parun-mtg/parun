module Parun.Types.Copyability where

-- | Whether a spell can be copied (CR 707.10), CR 113.6g's second half beside
-- Parun.Types.Counterability, and on the card for the reason that type gives:
-- SELF-referential, functioning on the stack, with no layer to fold through.
--
-- A field of its own and not a parameter on Counterability: each of
-- Parun.Types.Face's prohibitions gates one act at that act's site
-- (sacrificeRestrictions, untapRestrictions, keywordCostRestrictions), and the two
-- acts' carriers differ -- countering has a conditional form (Exquisite
-- Firecraft) and two foreign carriers (Parun.Types.PlayerEffect,
-- Parun.Types.ManaRider) that copying has no producer for.
--
-- Not a Bool, for Counterability's reason: CantBeCopied names the rule at the
-- site that reads it (Parun.Engine.Resolve.Arms.Objects.copyStackObject).
data Copyability
  = -- | CR 707.10: an effect may copy the spell.
    Copyable
  | -- | CR 113.6g: an effect that would copy the spell creates no copy.
    CantBeCopied
  deriving (Bounded, Enum, Eq, Ord, Show)
