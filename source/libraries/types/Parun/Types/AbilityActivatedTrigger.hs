module Parun.Types.AbilityActivatedTrigger where

import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | CR 602.2b / 601.2i: which activations fire "whenever [a player] activates
-- an ability [of a ...]". `filter` shadows the Prelude's, Parun.Types.Count's
-- naming.
data AbilityActivatedTrigger = MkAbilityActivatedTrigger
  { -- | Read against the ability, its controller the activator (CR 602.2a) and
    -- its source through Filter.FromSource.
    filter :: Filter.Filter Keyword.Keyword,
    -- | CR 605.1a's side the activation must be on: Nothing is Elrond,
    -- Moon-Reader's unnarrowed "an ability", Just NonManaAbility Runic
    -- Armasaur's "that isn't a mana ability".
    kind :: Maybe AbilityKind.AbilityKind
  }
  deriving (Eq, Ord, Show)
