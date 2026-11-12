module Parun.Types.AbilityFacts where

import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LoyaltyKind as LoyaltyKind

-- | What Parun.Types.ActivationCriteria asks of one activated ability, one field
-- per criterion: read off an activation in progress, a past one, the ability on
-- the stack (Filter.IsActivatedAbility) and the activation event.
data AbilityFacts = MkAbilityFacts
  { -- | Parun.Types.ActivatedAbility.keyword, for ActivationCriteria.grantedBy.
    keyword :: Maybe Keyword.Keyword,
    -- | Parun.Types.ActivatedAbility.name, for ActivationCriteria.whichAbility.
    name :: Maybe AbilityName.AbilityName,
    -- | CR 605.1a's side, for ActivationCriteria.whichKind.
    kind :: AbilityKind.AbilityKind,
    -- | CR 606.2's side, off the printed cost, for ActivationCriteria.whichLoyalty.
    loyalty :: LoyaltyKind.LoyaltyKind
  }
  deriving (Eq, Ord, Show)
