module Parun.Types.CandidateCost where

import qualified Control.Monad as Monad
import Numeric.Natural (Natural)
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.AppliedReduction as AppliedReduction
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.FreeCast as FreeCast
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaSpecification as ManaSpecification
import qualified Parun.Types.ObjectId as ObjectId

-- | CR 601.2b: one of the costs a spell may be cast for, together with WHAT
-- OFFERED IT.
--
-- The cost alone is not enough, because several rule 702 abilities condition
-- something AFTER the cast on which candidate was chosen: CR 702.34a's "if the
-- flashback cost was paid" and CR 702.133a's "if this spell was cast using its
-- jump-start ability" both exile the card as it leaves the stack, CR
-- 702.103b makes a spell cast bestowed an Aura enchantment, and CR 719.3b gives
-- a spell cast prototyped its inset frame's cost, box and colours. A bare
-- @Cost@ answers "what was paid" and never "which cost that was", and the two
-- come apart the moment a second permission offers the same card a second cost
-- from the same zone.
--
-- The tag is a Parun.Types.Keyword and not a Parun.Types.KeywordFamily because
-- jump-start and aftermath are NULLARY keywords, which that type deliberately
-- has no constructors for. Nothing means the candidate is the card's own
-- printed cost, one of its printed alternatives, or a cost an effect applied
-- (CR 118.9) -- in each case a cost no keyword ability offered.
--
-- Also what Prompt.ChooseCost offers and is answered with at every site, so
-- the answer names the candidate and not just its cost: Henzie "Toolbox"
-- Torre's blitz at a spell's own mana cost equals the printed cost and is a
-- different announcement (CR 601.2b, 702.152a). Sites where no keyword offers a
-- cost tag every candidate Nothing.
--
-- The keyword is the one READ IN THE ZONE the cast was proposed from (CR
-- 613.1), since that is where Parun.Engine.Cost.candidateCostsFor builds the
-- list: a granted flashback tags its candidate exactly as a printed one does.
data CandidateCost = MkCandidateCost
  { keyword :: Maybe Keyword.Keyword,
    cost :: Cost.Cost Keyword.Keyword,
    -- | CR 601.2f's reductions this candidate BRINGS WITH IT, on top of the ones
    -- the board states -- CR 702.119a's "if you chose to pay this spell's emerge
    -- cost, its total cost is reduced by an amount of generic mana equal to the
    -- sacrificed creature's mana value".
    --
    -- Here rather than folded into `cost`, because CR 601.2f applies every
    -- increase BEFORE any reduction: a candidate whose mana had the amount
    -- subtracted out would floor at {0} before a tax was added, and pay the tax in
    -- full. Parun.Engine.Cast.payableCostAt and Parun.Engine.Cast.castProposed both
    -- hand them to Parun.Engine.CostAdjustments.plusReductions, so the gate that offers the
    -- cast and the total that prices it cannot disagree.
    --
    -- Empty wherever the rule that offered it states no reduction. CR 702.48a's is the
    -- sacrificed permanent's whole mana cost, colored symbols included; a paid
    -- optional addition's keeps its own confinement (Defiler of Dreams).
    reductions :: [AppliedReduction.AppliedReduction],
    -- | CR 702.48a / 601.3c / 601.3d: this candidate may be announced any time
    -- its caster could cast an instant, whatever the card's own window.
    instantSpeed :: Bool,
    -- | CR 603.7g: the delayed ability a cast for this candidate creates as it
    -- becomes cast; Nothing for every candidate but the one a consequence-
    -- carrying CastingPermission.CastAsThoughItHadFlash offers.
    arms :: Maybe AbilityName.AbilityName,
    -- | CR 118.9 / 601.2b: which of the face's PRINTED alternative costs this
    -- candidate is, by index into Face.alternativeCosts; Nothing for every other
    -- candidate, a granted alternative included. Parun.Engine.Cast binds it under
    -- Binding.printedAlternativeCost, Baleful Mastery's "if the {1}{B} cost was
    -- paid".
    printedAlternative :: Maybe Natural,
    -- | CR 118.9: the alternative cost an effect applies (Omniscience, Leyline
    -- of Mutation), with the budget it spends: Just Nothing for an unlimited
    -- grant, Just its key under the granting object for a budgeted one (Warped
    -- Space's "Once each turn"); Nothing for every other candidate.
    granted :: Maybe (Maybe (ObjectId.ObjectId, FreeCast.FreeCast)),
    -- | CR 601.3d / 601.2e: the condition `instantSpeed`'s window holds under,
    -- re-asked once X is announced; Nothing where it holds under none.
    windowIf :: Maybe Condition.Condition,
    -- | CR 601.2h: the only mana that may pay this candidate, its alternative
    -- cost's spend rule (Parun.Types.AlternativeCost.spendOnly); Nothing for any.
    spendOnly :: Maybe ManaSpecification.ManaSpecification
  }
  deriving (Eq, Ord, Show)

-- | A candidate bringing no reduction, no window of its own, no delayed ability,
-- no printed alternative, no grant and no spend rule.
plain :: Maybe Keyword.Keyword -> Cost.Cost Keyword.Keyword -> CandidateCost
plain k c = MkCandidateCost k c [] False Nothing Nothing Nothing Nothing Nothing

-- | CR 702.48a / 601.3c / 601.3d: the candidate with a window opened under the
-- condition (Nothing: unconditionally) beside any it had.
widened :: Maybe Condition.Condition -> CandidateCost -> CandidateCost
widened condition candidate
  | instantSpeed candidate = candidate {windowIf = (\a b -> Condition.Any [a, b]) <$> windowIf candidate <*> condition}
  | otherwise = candidate {instantSpeed = True, windowIf = condition}

-- | CR 118.9: the budgeted grant this candidate spends, recorded in
-- Parun.Types.TurnRegisters.freeCastsUsedThisTurn once the spell becomes cast.
spends :: CandidateCost -> Maybe (ObjectId.ObjectId, FreeCast.FreeCast)
spends = Monad.join . granted
