module Parun.Types.CostModifier where

import qualified Parun.Types.CostChange as CostChange
import qualified Parun.Types.CostSubject as CostSubject
import qualified Parun.Types.CountedAgainst as CountedAgainst
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PerTarget as PerTarget
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TurnScope as TurnScope

-- | The payload of Parun.Types.PlayerEffect's ModifyCost arm: CR 601.2f's
-- increase, reduction or additional cost, applied by a CR 613.11 effect to the
-- spells or the activations its subject names (Thalia, Sapphire Medallion,
-- Drought, Oppressive Rays, Heartstone, Brutal Suppression).
--
-- ONE record for both subjects because CR 602.2b applies CR 601.2b-i to an
-- activation "just as they apply to casting a spell": every criterion below
-- reads a step of that shared procedure, so it reads the same for either.
-- What only an activation has -- an ability among several on one object --
-- rides the subject (Parun.Types.ActivationCriteria).
data CostModifier = MkCostModifier
  { -- | Spells, or activated abilities and what narrows them.
    subject :: CostSubject.CostSubject,
    -- | The spell, or the activated ability's SOURCE object, matched against
    -- its projection: Thalia's "noncreature spells", Heartstone's "activated
    -- abilities of creatures".
    matching :: Filter.Filter Keyword.Keyword,
    -- | CR 601.2c: when Just, applies only where SOME announced target matches
    -- -- Kopala, Warden of Waves' "that target a Merfolk you control", Dwarven
    -- Mauler's "that target this creature". Once, however many match.
    whichTargets :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 601.2c: when Just, the change applies once per DISTINCT announced
    -- target this matches, or as many times as its `each` reads against that
    -- target -- Hinata, Dawn-Crowned's "{1} more to cast for each target",
    -- Battlefield Thaumaturge's "for each creature it targets", Warrior's
    -- Blades' "for each +1/+1 counter on the creature it targets".
    perTarget :: Maybe PerTarget.PerTarget,
    -- | CR 601.2f: when Just, the change applies this many times, read as the
    -- total is determined against `countedAgainst` -- Henzie "Toolbox"
    -- Torre's "{1} less for each time you've cast your commander". Multiplies
    -- `perTarget`'s count where both are given. A resolving effect storing the
    -- change fixes it then (CR 608.2h), Mutated Cultist's "for each counter
    -- removed this way"; its scenarios prove it.
    perEach :: Maybe Quantity.Quantity,
    -- | What `perEach` is counted against, and whose "you" it reads.
    countedAgainst :: CountedAgainst.CountedAgainst,
    -- | Professor Hojo's "the FIRST activated ability you activate during your
    -- turn" and Shadow in the Warp's "the first creature spell you cast each
    -- turn": Just a scope applies the change only in a turn the scope admits,
    -- and only to the first matching activation or cast the player begins that
    -- turn -- whether or not that one was changed, and whether or not this
    -- effect existed yet (Tezzeret, Betrayer of Flesh's and Shadow in the
    -- Warp's rulings).
    onlyFirst :: Maybe TurnScope.TurnScope,
    -- | What it does to the total cost.
    change :: CostChange.CostChange
  }
  deriving (Eq, Ord, Show)
