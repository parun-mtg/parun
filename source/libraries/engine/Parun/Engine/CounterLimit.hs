-- CR 122.4 / 704.5r: the counter limits a permanent's own abilities state. The
-- only reader of Parun.Types.CounterLimit; its one caller, Parun.Engine.Sba,
-- learns how many counters of which kind go and never which card said so.
module Parun.Engine.CounterLimit where

import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLimit as CounterLimit
import Parun.Types.GameState (GameState)
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.RuleAbilities as RuleAbilities

-- CR 704.5r: each kind of counter `oid` holds more of than an ability of its
-- own allows, with how many of them go. Two limits on one kind keep the
-- smaller N, since each ability removes down to its own.
--
-- Read through the projection, as the ruling of 2009-10-01 on Rasputin
-- Dreamweaver requires: the copiable values (Projection.ruleAbilitiesOf) carry
-- the limit to a copy, and a permanent whose abilities CR 613.1f or CR 305.7
-- removed has none, so it keeps its counters. A permanent with no counters,
-- every permanent in almost every game, is never projected.
excess :: ObjectId -> GameState -> [(CounterKind.CounterKind Keyword.Keyword, Natural)]
excess oid gs = case Object.counters <$> Game.lookupObject oid gs of
  Just held | not (Map.null held) -> case limits of
    [] -> []
    _ ->
      let mosts = Map.fromListWith min [(CounterLimit.kind limit, CounterLimit.atMost limit) | limit <- limits]
          over kind most = case Map.lookup kind held of
            Just count | count > most -> Just (count - most)
            _ -> Nothing
       in Map.toList (Map.mapMaybeWithKey over mosts)
  _ -> []
  where
    setEffs = Projection.setLandSubtypeEffects gs
    -- CR 613.1f: a stored grant's rows, whose gates
    -- Projection.grantedRuleAbilities has asked already.
    granted = RuleAbilities.counterLimits (Projection.grantedRuleAbilities gs oid)
    printed = case RuleAbilities.counterLimits (Projection.ruleAbilitiesOf oid gs) of
      [] -> []
      rows
        | (null setEffs || Projection.liveAfterLayers setEffs oid gs) && not (Projection.abilityRemoval gs oid) -> rows
        | otherwise -> []
    limits = printed <> granted
