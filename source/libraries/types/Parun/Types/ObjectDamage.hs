module Parun.Types.ObjectDamage where

import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | The payload of Parun.Types.Quantity's DamageDealtThisTurn arm.
data ObjectDamage = MkObjectDamage
  { -- | Which side of the damage the object is on (CR 120.1).
    direction :: DamageDirection.DamageDirection,
    -- | CR 120.1 / 608.2h: only damage whose source, as it dealt the damage,
    -- matches (Grothama, All-Devouring's "by sources they controlled", Blazing
    -- Effigy's "by other sources named Blazing Effigy"), or any damage when
    -- Nothing.
    by :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
