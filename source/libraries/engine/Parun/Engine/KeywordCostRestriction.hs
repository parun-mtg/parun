-- CR 702.122d / 702.196b / 101.2 / 613.11: the continuous effects that FORBID
-- a creature to pay part of a keyword's cost -- tapping it to crew, returning it
-- to beam a spell up. One of the modules on the axis CR 613.11 reaches past the
-- layer system (alongside Parun.Engine.PlayerEffect,
-- Parun.Engine.BlockRequirement, Parun.Engine.AttackRequirement,
-- Parun.Engine.CombatRestriction, Parun.Engine.AttackCost,
-- Parun.Engine.SacrificeRestriction and Parun.Engine.UntapRestriction). None is a
-- layer, and no layer of Parun.Engine.Projection rewrites them.
--
-- The only reader of Parun.Types.KeywordCostRestriction. Its caller asks for a
-- SET OF IDS and never learns which card produced it.
--
-- ONE place asks, unlike Parun.Engine.SacrificeRestriction's two, and the reason
-- is that both rules name a COST rather than a game action: a creature is only
-- ever tapped to crew by paying rule 702.122a's cost, or returned to beam up by
-- paying rule 702.196a's, and Parun.Engine.Cost.tapCandidates (aliased as
-- returnCandidates) is the single pool that both CR 118.3's payability gate and
-- the payment prompt read.
module Parun.Engine.KeywordCostRestriction where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.Rewrite as Projection
import qualified Parun.Engine.Projection.View as Projection
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.KeywordCostPart as KeywordCostPart
import qualified Parun.Types.KeywordCostRestriction as KeywordCostRestriction
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.RuleAbilities as RuleAbilities

-- CR 702.122d / 702.196b with CR 101.2: which of `candidates` an effect in force
-- right now says can't pay `part` of its keyword's cost. Revoke Privileges' third
-- clause and Marooned's last are the pool's printings;
-- `keywordCostRestrictions` in data/cards is the carrier, and this answers for
-- every card that fills it.
--
-- Parun.Engine.SacrificeRestriction.cantBeSacrificed's body, gate for gate, and
-- every step of its argument holds here unchanged: the same CR 305.7 and CR
-- 613.1f ability losses are asked of the SOURCE, the same CR 612.1 word swap is
-- applied to its affected set, and CR 613.11 is why the set is read against the
-- FULL projection rather than a layer-bounded one.
prohibited :: KeywordCostPart.KeywordCostPart -> [ObjectId] -> GameState -> Set ObjectId
prohibited part candidates gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
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
      ofPart = filter ((== part) . KeywordCostRestriction.part) . RuleAbilities.keywordCostRestrictions
      fromGrant source = concatMap (fromRestriction source []) (ofPart (grantedRules source))
      fromPermanent source = case ofPart (Projection.ruleAbilitiesOf source gs) of
        -- Every permanent in almost every game.
        [] -> []
        restrictions ->
          if (null setEffs || Projection.liveAfterLayers setEffs source gs)
            && not (removed source)
            then concatMap (fromRestriction source (Projection.readerTextChanges source gs)) restrictions
            else []
      fromRestriction source changes restriction =
        let affected = KeywordCostRestriction.affected restriction
         in filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
   in Set.fromList (concatMap (\source -> fromPermanent source <> fromGrant source) (Set.toList (GameState.battlefield gs)))
