module Parun.Types.CostSubject where

import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.Keyword as Keyword

-- | Which payments a Parun.Types.CostModifier reaches. The constructor and not
-- the Filter decides it, since nothing in a Filter can say "and it is a spell":
-- that is what keeps Thalia off Mindslaver's activation (#90).
data CostSubject
  = -- | CR 601.2f: casting a spell.
    Spells
  | -- | CR 602.2b: activating an ability, narrowed by what only an ability has.
    Activations (ActivationCriteria.ActivationCriteria Keyword.Keyword)
  deriving (Eq, Ord, Show)
