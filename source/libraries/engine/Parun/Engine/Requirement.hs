-- CR 508.1d / 509.1c: the shape both combat requirement gatherers answer in, and
-- the one thing Parun.Engine.Combat's two maximizations have in common. Neither
-- rule is about a card: both count REQUIREMENTS a declaration obeys, and this
-- module is that count and nothing else.
--
-- Parametric in the declaration's atom -- a (blocker, attacker) pair on
-- Parun.Engine.BlockRequirement's side, a (creature, attack target) pair on
-- Parun.Engine.AttackRequirement's -- because the counting argument is the same on
-- both and the atom is the only difference.
module Parun.Engine.Requirement where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

-- CR 508.1d / 509.1c's requirements in force, instantiated against the atoms a
-- declaration is made of, in the TWO arities the printings use
-- (Parun.Types.RequirementArity).
--
-- `pairs` is one requirement per atom, weighted by how many name it -- Lure over
-- three able creatures is three requirements, and a Razorgrass Screen under a
-- Lure is two on one pair.
--
-- `groups` is one requirement over a SET of atoms, obeyed by a declaration
-- containing ANY of them -- Gaea's Protector's "this creature must be blocked if
-- able", Seeker of Slaanesh's "each opponent must attack with at least one
-- creature each combat if able". Keyed by the set, so two permanents stating the
-- same group are one shape carrying two: that is what keeps the attacking
-- maximization's cost in the number of DISTINCT groups rather than in the number
-- of permanents.
--
-- `exactly` is a group obeyed by a declaration containing EXACTLY ONE of its
-- atoms -- Nacatl War-Pride's "must be blocked by exactly one creature if able"
-- (Parun.Types.RequiredBlockers.ExactlyOne). Unlike the other two, a larger
-- declaration can obey it less. Only the blocking side states one, so the
-- attacking maximization never reads it.
data Instances key = MkInstances
  { pairs :: Map key Natural,
    groups :: Map (Set key) Natural,
    exactly :: Map (Set key) Natural
  }
  deriving (Eq, Ord, Show)

-- The three multisets built at once, from one gathering walk's outputs.
--
-- An EMPTY group, of either kind, is dropped, which is the "if able" of "must
-- be blocked if able": a requirement no declaration can obey raises the maximum by nothing.
-- Dropping it changes no answer -- 'met' would never count it either -- but it
-- keeps 'vacuous' honest, so a board where nothing is able skips the search
-- entirely, the pruning posture Parun.Engine.BlockRequirement.instances takes
-- with its `able` predicate. Mutating the drop away leaves the Combat subtree
-- green, and this states why.
gather :: (Ord key) => [key] -> [Set key] -> [Set key] -> Instances key
gather atoms sets exactSets =
  MkInstances
    { pairs = Map.fromListWith (+) (fmap (\atom -> (atom, 1)) atoms),
      groups = multiset sets,
      exactly = multiset exactSets
    }
  where
    multiset these = Map.fromListWith (+) (fmap (\set -> (set, 1)) (filter (not . Set.null) these))

-- `required` plus one requirement on each of `atoms` heavier than all of
-- `required` together, and that weight: a maximum reaching it times the atoms
-- held, plus the old maximum, holds them all and obeys as many as before
-- (Parun.Engine.Combat's couldAttack, attackCreaturesAllowedGiven and
-- declarableBlockers).
boosted :: (Ord key) => [key] -> Instances key -> (Natural, Instances key)
boosted atoms required =
  let heavier = 1 + sum (pairs required) + sum (groups required) + sum (exactly required)
   in (heavier, required {pairs = Map.unionWith (+) (Map.fromList (fmap (\atom -> (atom, heavier)) atoms)) (pairs required)})

-- Is nothing in force at all? The board almost every game is played on, and what
-- lets both maximizations skip their search entirely.
vacuous :: Instances key -> Bool
vacuous instances = Map.null (pairs instances) && Map.null (groups instances) && Map.null (exactly instances)

-- CR 508.1d / 509.1c: how many requirements this declaration obeys. Summing
-- multiplicities rather than counting keys, because both rules count
-- REQUIREMENTS: two naming one atom are both obeyed by declaring it.
met :: (Ord key) => Instances key -> Set key -> Natural
met instances declaration =
  sum (Map.filterWithKey (\atom _ -> Set.member atom declaration) (pairs instances))
    + groupsMet instances declaration
    + exactlyMet instances declaration

-- The group half of 'met' alone, which is what a search that tracks the two
-- halves separately adds as it pins a witness.
groupsMet :: (Ord key) => Instances key -> Set key -> Natural
groupsMet instances declaration =
  sum (Map.filterWithKey (\set _ -> not (Set.disjoint set declaration)) (groups instances))

-- The `exactly` half of 'met': the groups holding exactly one declared atom.
exactlyMet :: (Ord key) => Instances key -> Set key -> Natural
exactlyMet instances declaration =
  sum (Map.filterWithKey (\set _ -> Set.size (Set.intersection set declaration) == 1) (exactly instances))

-- The most the `exactly` groups could still add once atoms are only ever added
-- to this declaration: those holding at most one of its atoms. A group holding
-- two is lost for good, so this bound is admissible.
exactlyLive :: (Ord key) => Instances key -> Set key -> Natural
exactlyLive instances declaration =
  sum (Map.filterWithKey (\set _ -> Set.size (Set.intersection set declaration) <= 1) (exactly instances))

-- The most the groups NOT yet obeyed could still add. An admissible bound for a
-- search that prunes on one, since obeying a group is worth its weight once
-- however many of its atoms the declaration ends up containing.
unobeyed :: (Ord key) => Instances key -> Set (Set key) -> Natural
unobeyed instances obeyed =
  sum (Map.filterWithKey (\set _ -> not (Set.member set obeyed)) (groups instances))

-- The groups any of these atoms would newly obey, as the sets themselves so a
-- caller can accumulate the obeyed ones without an index.
covering :: (Ord key) => Instances key -> Set (Set key) -> [key] -> [(Set key, Natural)]
covering instances obeyed atoms =
  Map.toList
    ( Map.filterWithKey
        (\set _ -> not (Set.member set obeyed) && any (\atom -> Set.member atom set) atoms)
        (groups instances)
    )
