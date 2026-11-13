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

-- | Whether a deck of this family is built from cards its player drafted, so
-- that its sideboard is what the player drafted and left out (CR 100.4b,
-- 903.13a; docs/decisions.md, "Which cards a player drafted that aren't in
-- their deck").
drafted :: RuleFamily.RuleFamily -> Bool
drafted family = case family of
  RuleFamily.Constructed -> False
  RuleFamily.Limited -> True
  RuleFamily.Commander -> False
  RuleFamily.Brawl -> False
  RuleFamily.CommanderDraft -> True
