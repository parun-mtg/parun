module Parun.Types.KeywordCostRestriction where

import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.KeywordCostPart as KeywordCostPart

-- | CR 101.2: one printed KEYWORD COST PROHIBITION -- an effect saying a
-- creature can't pay part of a rule-702 keyword's cost: "can't crew Vehicles"
-- (CR 702.122d; Revoke Privileges, Bound in Gold, Intercessor's Arrest) or
-- "can't be beamed up" (CR 702.196b; Marooned).
--
-- Parun.Types.SacrificeRestriction's shape and its filing, one game action over:
-- CR 613.11 puts a continuous effect that "affects game rules rather than
-- objects" outside the layer system, CR 101.2a holds rule 101.2 clear of adding
-- abilities to objects and removing them, and no layer of Parun.Engine.Projection
-- rewrites them. Every step of that type's argument for why it cannot be a
-- Parun.Types.Modification holds here unchanged.
--
-- CR 101.2 is what gives the prohibition its force over the keyword's cost: the
-- keyword's ability directs a tap (CR 702.122a) or a return (CR 702.196a) and
-- this states it can't happen, so the "can't" wins and CR 118.3 then makes that
-- cost unpayable by the creature.
--
-- No "unless" gate beside the subject: the pool's printings state none, and
-- neither CR 702.122d nor CR 702.196b writes a clause into the rule the way CR
-- 508.1c does.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture every sibling carrier takes: a Revoke Privileges that left lifts its
-- prohibition with nothing to unwind.
--
-- THE KEYWORD'S COST ALONE, which is what each rule names. The same
-- Parun.Types.CostComponent that pays the keyword's cost is printable outside
-- one (data/cards/synthetic-crewed-battery.json, Meloku the Clouded Mirror), so
-- the narrowing is made where the KEYWORD is known: Parun.Engine.Keyword and
-- Parun.Engine.Cost's keyword candidates are the only minters of
-- Parun.Types.Filter's CantPayKeywordCost atom, and
-- Parun.Engine.Cost.tapCandidates is what supplies the sets the atom reads.
--
-- Open-half card data, classified rather than identified:
-- Parun.Engine.KeywordCostRestriction is the only module that may read it, and
-- it answers a set of ids.
data KeywordCostRestriction = MkKeywordCostRestriction
  { -- | Which keyword's cost the creatures can't pay part of.
    part :: KeywordCostPart.KeywordCostPart,
    -- | Which creatures are prohibited. An Affected, not a bare ObjectId,
    -- so the set is re-derived every time it is asked -- CR 611.3a's static
    -- effect is not locked in, which is what Revoke Privileges' set shows as the
    -- Aura moves.
    affected :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
