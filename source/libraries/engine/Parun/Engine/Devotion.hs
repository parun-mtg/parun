module Parun.Engine.Devotion where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility

-- CR 700.5a: how much these player abilities raise their controller's devotion
-- to each colour and combination of colours. A classification of one
-- Parun.Types.PlayerEffect arm, written below Parun.Engine.Projection.View and
-- Parun.Engine.Count so that both can fill Filter.devotionBonus from it, since
-- neither may import Parun.Engine.PlayerEffect.
--
-- Only a printed "your devotion" with no "as long as" counts: Altar of the
-- Pantheon is every printing, and Parun.EffectLintSpec's
-- devotionIncreaseOffends rejects any other scope, a condition or a stored
-- one, none of which one permanent's view could resolve.
bonusOf :: [PlayerStaticAbility.PlayerStaticAbility] -> Natural.Natural
bonusOf = sum . fmap bonus
  where
    bonus ability = case (PlayerStaticAbility.scope ability, PlayerStaticAbility.condition ability, PlayerStaticAbility.effect ability) of
      (PlayerScope.Related PlayerRelation.You, Nothing, PlayerEffect.IncreaseDevotion n) -> n
      _ -> 0
