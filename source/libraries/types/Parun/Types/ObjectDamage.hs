module Parun.Types.ObjectDamage where

import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Quantity's DamageDealtThisTurn arm.
data ObjectDamage = MkObjectDamage
  { -- | Which side of the damage the object is on (CR 120.1).
    direction :: DamageDirection.DamageDirection,
    -- | CR 120.1 / 608.2h: only damage whose source, as it dealt the damage,
    -- matches (Grothama, All-Devouring's "by sources they controlled", Blazing
    -- Effigy's "by other sources named Blazing Effigy"), or any damage when
    -- Nothing.
    by :: Maybe (Filter.Filter Keyword.Keyword),
    -- | Nothing for any damage; Just Noncombat for CR 120.2b's alone (Temple of
    -- Power's "dealt 4 or more noncombat damage").
    kind :: Maybe DamageKind.DamageKind,
    -- | CR 120.1: beside DealtBy, only damage dealt to the players this names
    -- (Vessel of the All-Consuming's "to that player"), or to anything when
    -- Nothing. Beside DealtTo the object is the recipient, so no card writes it
    -- (Parun.EffectLintSpec).
    to :: Maybe PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
