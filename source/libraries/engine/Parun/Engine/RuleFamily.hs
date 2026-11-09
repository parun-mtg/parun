-- Facts about a rule family of CR 100.2 that more than one reader needs: deck
-- legality before a game, and companion conditions inside one.
module Parun.Engine.RuleFamily where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.RuleFamily as RuleFamily

-- | The minimum deck size a rule family sets: CR 100.2a, 100.2b, 903.5a,
-- 903.12d and 903.13f(1).
minimumDeckSize :: RuleFamily.RuleFamily -> Natural.Natural
minimumDeckSize family = case family of
  RuleFamily.Constructed -> 60
  RuleFamily.Limited -> 40
  RuleFamily.Commander -> 100
  RuleFamily.Brawl -> 60
  RuleFamily.CommanderDraft -> 60
