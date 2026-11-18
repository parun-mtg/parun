-- CR 702.3b / 302.6 / 514.2 / 613.11: the continuous effects that let a creature
-- attack past defender or summoning sickness, have its {T} and {Q} abilities
-- activated past summoning sickness, or keep its damage through cleanup. One of
-- the modules on the axis CR 613.11 reaches past the layer system, alongside
-- Parun.Engine.BlockPermission and Parun.Engine.CombatRestriction; none is a
-- layer.
--
-- The only reader of Parun.Types.RulePermission. Parun.Engine.Combat,
-- Parun.Engine.Cost and Parun.Engine.Damage ask for a map of waivers and never
-- learn which card produced one.
module Parun.Engine.RulePermission where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Parun.Engine.Condition as Condition
import qualified Parun.Engine.PlayerEffect as PlayerEffect
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.Rewrite as Projection
import qualified Parun.Engine.Projection.View as Projection
import qualified Parun.Types.AimedAt as AimedAt
import qualified Parun.Types.AimedPlayers as AimedPlayers
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import Parun.Types.ObjectId (ObjectId)
import Parun.Types.PlayerId (PlayerId)
import qualified Parun.Types.RuleAbilities as RuleAbilities
import qualified Parun.Types.RulePermission as RulePermission
import Parun.Types.RuleWaiver (RuleWaiver)

-- CR 702.3b / 302.6: which of `candidates` an effect in force right now lifts
-- which rule for, and at which CR 508.1b announcements: Nothing is every one,
-- Just the (defending player, kind) pairs a target-bound permission names
-- (Frenzied Saddlebrute). Two permissions lifting one rule reach the union, an
-- unbound one absorbing the rest. Parun.Engine.KeywordCostRestriction.prohibited's body,
-- gate for gate: the same CR 305.7 and CR 613.1f ability losses asked of the
-- source, the same CR 612.1 word swap over its affected set, read against the
-- FULL projection (CR 613.11).
waivers :: [ObjectId] -> GameState -> Map (ObjectId, RuleWaiver) (Maybe (Set (PlayerId, AttackTargetKind.AttackTargetKind)))
waivers candidates gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      named source affected candidate = Projection.affectsOn pcs grants source candidate affected gs
      -- CR 613.1f: what a stored grant gave the permanent (Prison Barricade's
      -- kicked entry), with no CR 612.1 word swap (CR 612.3).
      grantedRules = Projection.grantedRuleAbilities gs
      fromGrant source = concatMap (fromPermission source []) (RuleAbilities.rulePermissions (grantedRules source))
      fromPermanent source = case RuleAbilities.rulePermissions (Projection.ruleAbilitiesOf source gs) of
        -- Every permanent in almost every game.
        [] -> []
        permissions ->
          if (null setEffs || Projection.liveAfterLayers setEffs source gs)
            && not (removed source)
            then concatMap (fromPermission source (Projection.readerTextChanges source gs)) permissions
            else []
      -- The "as long as" (CR 611.3a), BlockPermission's `granted`: asked once
      -- per permission, against the source, since CR 109.5 fixes the "you"
      -- inside it.
      granted source changes permission = case RulePermission.while permission of
        Nothing -> True
        Just condition ->
          Condition.holds
            (Projection.fullView gs)
            (Projection.sourceContext gs (Projection.controllerOf source gs) source)
            gs
            source
            (if null changes then condition else Projection.rewriteCondition changes condition)
      -- CR 109.5: the "you" of "your opponents" is the source's controller. A
      -- slot names nobody, a static ability having no resolution to fill one.
      reach source permission = case RulePermission.against permission of
        Nothing -> Nothing
        -- Unprinted: a permanents filter on a waiver would need a reach
        -- keyed by permanent; Parun.FilterPositionLintSpec's "CR 508.1c no
        -- card writes a permanents filter on a rule permission" keeps it out.
        Just (AimedAt.MkAimedAt defenders kinds _) ->
          let players = case (defenders, Projection.controllerOf source gs) of
                (AimedPlayers.Scoped scope, Just you) -> filter (\pid -> PlayerEffect.inScopeWithin (Just source) (Just you) pid you gs scope) (GameState.seating gs)
                (AimedPlayers.BoundPlayer bound, _) -> [bound]
                _ -> []
           in Just (Set.fromList ((,) <$> players <*> Set.toList kinds))
      fromPermission source changes permission =
        let affected = RulePermission.affected permission
         in if granted source changes permission
              then (\candidate -> ((candidate, RulePermission.waives permission), reach source permission)) <$> filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
              else []
   in Map.fromListWith (liftA2 Set.union) (concatMap (\source -> fromPermanent source <> fromGrant source) (Set.toList (GameState.battlefield gs)))
