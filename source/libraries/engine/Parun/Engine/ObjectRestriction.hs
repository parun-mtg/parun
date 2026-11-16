-- CR 101.2 / 613.11: the continuous effects that FORBID a permanent one game
-- action -- a sacrifice (CR 701.21a), being turned face up (CR 708) or gaining
-- a designation (CR 701.60a).
-- One of the modules on the axis CR 613.11 reaches past the layer system
-- (alongside Parun.Engine.PlayerEffect, Parun.Engine.BlockRequirement,
-- Parun.Engine.AttackRequirement, Parun.Engine.CombatRestriction and
-- Parun.Engine.AttackCost). None is a layer, and no layer of
-- Parun.Engine.Projection rewrites them.
--
-- The only reader of Parun.Types.ObjectRestriction. Every caller asks for a
-- SET OF IDS -- or for one id's answer -- and never learns which card produced
-- it.
--
-- WHERE THE ANSWER IS ASKED is two kinds of place per action, and both are
-- needed. For a sacrifice, CR 101.2 makes the "can't" beat the "can", so a
-- prohibited permanent must
-- neither be OFFERED as a sacrifice (Parun.Engine.Replacement.sacrificeCandidates,
-- which is what CR 118.3's payability count and every sacrifice prompt read)
-- nor be sacrificed by an instruction that named it without asking
-- (Parun.Engine.Event.sacrificeIn, the CR 701.21 funnel). Gating only the first
-- would leave every targeted sacrifice in the pool going through. For turning
-- face up, likewise: CR 116.2b's special action must not be offered
-- (Parun.Engine.FaceDown.canTurnFaceUp) and an effect must not turn the
-- permanent over (Parun.Engine.Event.turnFaceUpByEffect). Gaining a designation
-- is never offered, so its one place is the Effect.Designate arm
-- (Parun.Engine.Resolve.Arms.Objects.designate).
module Parun.Engine.ObjectRestriction where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.Rewrite as Projection
import qualified Parun.Engine.Projection.View as Projection
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.ObjectRestriction as ObjectRestriction
import qualified Parun.Types.RestrictedAction as RestrictedAction
import qualified Parun.Types.RuleAbilities as RuleAbilities

-- CR 101.2: which of `candidates` an effect in force right now says can't
-- undergo `action` -- Garland, Royal Kidnapper's "can't be sacrificed", Unable
-- to Scream's "can't be turned face up", Airtight Alibi's "can't become
-- suspected".
--
-- A set of ids and not a per-candidate predicate, for the reason
-- Parun.Engine.CombatRestriction.restricted gives: a caller narrowing a whole
-- battlefield asks this once and tests against the answer, where a predicate
-- would re-walk the battlefield per candidate.
restricted :: RestrictedAction.RestrictedAction -> [ObjectId] -> GameState -> Set ObjectId
restricted action candidates gs =
  let -- Hoisted out of the walk as AttackRequirement.instances hoists them, and
      -- both unforced until some permanent actually declares a prohibition.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of Projection.affects's
      -- callers inside the layer fold, which read characteristics as of their
      -- own layer.
      named source affected candidate =
        Projection.affectsOn
          pcs
          grants
          source
          candidate
          affected
          gs
      -- CR 613.1f: what a stored grant gave the permanent, whose gates
      -- Projection.grantedRuleAbilities has asked already, with no CR 612.1
      -- word swap (CR 612.3) -- CombatRestriction.gathered's reading.
      grantedRules = Projection.grantedRuleAbilities gs
      fromGrant source = concatMap (fromRestriction source []) (RuleAbilities.objectRestrictions (grantedRules source))
      fromPermanent source = case RuleAbilities.objectRestrictions (Projection.ruleAbilitiesOf source gs) of
        -- Every permanent in almost every game.
        [] -> []
        restrictions ->
          -- The same two ability losses AttackRequirement.instances asks
          -- about: CR 305.7's basic-land subtype set, and CR 604.2 against a
          -- CR 613.1f layer-6 removal. Why CR 613.6 cannot rescue a
          -- restriction that has started to apply is argued in
          -- BlockRequirement.instances, as is why CR 613.11 also lets the CR
          -- 305.7 gate be liveAfterLayers rather than liveGiven.
          if (null setEffs || Projection.liveAfterLayers setEffs source gs)
            && not (removed source)
            then
              -- CR 612.1's word swap over the SOURCE's own text, computed here
              -- rather than hoisted beside setEffs, the placement
              -- CombatRestriction.restricted argues for: readerTextChanges
              -- folds the whole continuous-effect list, and the empty case
              -- above already turned away every permanent that prints no
              -- prohibition.
              concatMap (fromRestriction source (Projection.readerTextChanges source gs)) restrictions
            else []
      fromRestriction source changes restriction
        | ObjectRestriction.action restriction /= action = []
        | otherwise =
            let affected = ObjectRestriction.affected restriction
             in filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
   in Set.fromList (concatMap (\source -> fromPermanent source <> fromGrant source) (Set.toList (GameState.battlefield gs)))

-- The same question about ONE permanent, for the callers that hold a victim
-- rather than a candidate list: the CR 701.21 funnel itself, the two cost
-- components that name the permanent paying with itself, both roads that
-- turn a permanent face up, and the designate arm.
--
-- Not a cheaper computation, just a narrower one -- the walk above is over the
-- battlefield either way, and the candidate list it filters is the singleton.
prohibited :: RestrictedAction.RestrictedAction -> ObjectId -> GameState -> Bool
prohibited action oid gs = Set.member oid (restricted action [oid] gs)
