-- CR 106.1: the one place a Parun.Types.ManaFilter is interpreted. A predicate
-- over one unit of mana, the mana-side counterpart of
-- Parun.Engine.Filter.matches -- and, like it, identity-blind: it never learns
-- which effect or card asked.
module Parun.Engine.ManaFilter where

import qualified Data.Set as Set
import qualified Parun.Types.ManaFilter as ManaFilter
import qualified Parun.Types.ManaType as ManaType
import Parun.Types.ManaUnit (ManaUnit)
import qualified Parun.Types.ManaUnit as ManaUnit
import qualified Parun.Types.ProductionTag as ProductionTag

-- | CR 106.1b / 106.3: mana of this type whose production carried these tags.
-- Kept apart from a pool unit because a SUPPLY (Parun.Engine.Mana.Supply) may
-- not have settled on a type yet, so the filter has to be askable of a bare
-- type and its tags as well.
matchesMade :: ManaFilter.ManaFilter -> Set.Set ProductionTag.ProductionTag -> ManaType.ManaType -> Bool
matchesMade filter_ tags manaType = case filter_ of
  ManaFilter.Any -> True
  ManaFilter.OfType wanted -> manaType == wanted
  ManaFilter.NotOfType unwanted -> manaType /= unwanted
  ManaFilter.Produced tag -> Set.member tag tags

-- | The same question about a bare type, which carries no tag: CR 106.7's
-- "could produce" and a reduction's confinement, whose codecs refuse a Produced
-- filter (Parun.Codec.CouldProduce, Parun.Codec.AppliedReduction).
matchesType :: ManaFilter.ManaFilter -> ManaType.ManaType -> Bool
matchesType filter_ = matchesMade filter_ Set.empty

-- | The same question about a unit in a pool.
matches :: ManaFilter.ManaFilter -> ManaUnit -> Bool
matches filter_ unit = matchesMade filter_ (ManaUnit.tags unit) (ManaUnit.manaType unit)
