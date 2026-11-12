module Parun.Types.ActivationCriteria where

import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.LoyaltyKind as LoyaltyKind

-- | What narrows a Parun.Types.CostModifier to SOME of the activated abilities
-- of a matching source. Each is a fact about the ABILITY being activated, which
-- the source Filter, asked of the object, cannot answer -- and none has a spell
-- counterpart, a spell being one object with one cost.
--
-- Nothing in every field is every activated ability of a matching source.
--
-- PARAMETRIC in the keyword for Parun.Types.Filter's reason:
-- Filter.IsActivatedAbility carries one, and Keyword names Filter. Only
-- @ActivationCriteria Keyword.Keyword@ is ever written.
data ActivationCriteria keyword = MkActivationCriteria
  { -- | Only an ability under this rule-702 keyword (Parun.Types.ActivatedAbility.keyword):
    -- Fluctuator's "cycling abilities", Bureau Headmaster's "equip abilities".
    -- Spell-free because a spell's keywords are the spell's, which the Filter
    -- reads.
    grantedBy :: Maybe (KeywordDesignator.KeywordDesignator keyword),
    -- | CR 605.1a: Zirda, the Dawnwaker's "that aren't mana abilities",
    -- Suppression Field's "unless they're mana abilities". Spell-free because
    -- rule 605.1a classifies only abilities.
    whichKind :: Maybe AbilityKind.AbilityKind,
    -- | CR 606.2: Carth the Lion's "planeswalkers' loyalty abilities". Spell-free
    -- because rule 606.2 classifies only activated abilities.
    whichLoyalty :: Maybe LoyaltyKind.LoyaltyKind,
    -- | Only the ability of this Parun.Types.ActivatedAbility.name: Sanctum of
    -- Tranquil Light's "this ability costs {1} less" (CR 602.2b, 601.2f).
    whichAbility :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
