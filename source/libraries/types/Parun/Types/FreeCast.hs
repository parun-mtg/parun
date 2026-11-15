module Parun.Types.FreeCast where

import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.PermissionLimit as PermissionLimit
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TurnOrdinal as TurnOrdinal
import qualified Parun.Types.Zone as Zone

-- | The payload of Parun.Types.PlayerEffect's PayRatherThanManaCost arm:
-- which spells CR 118.9's grant covers, what it has their caster pay instead,
-- the number their filter's EnclosingAmount operand compares against, the zone
-- it is confined to, which cast of a turn it reaches, and how often it may be
-- used.
data FreeCast = MkFreeCast
  { -- | Read off the card's CR 613.1 view where it is cast from, as
    -- Parun.Types.CastFromZone's `matching` is.
    spells :: Filter.Filter Keyword.Keyword,
    -- | CR 118.9: the mana paid in place of the mana cost, Leyline of
    -- Mutation's {W}{U}{B}{R}{G}; empty, {0} (CR 118.5), where the grant pays
    -- no mana (Omniscience).
    mana :: ManaCost.ManaCost,
    -- | CR 118.9: the non-mana components paid in place of the mana cost, read
    -- against the SPELL being cast, Dream Halls' "a card that shares a color
    -- with that spell"; empty where the grant pays nothing (Omniscience).
    instead :: [CostComponent.CostComponent Keyword.Keyword],
    -- | CR 109.5: read for the grant's SOURCE, as Parun.Types.AffectedUnless's
    -- `amount` is (Omnipresence's "the number of creatures you control").
    -- Nothing where the filter compares against no computed number.
    amount :: Maybe Quantity.Quantity,
    -- | The grantee's own zone the cast must begin in, Omniscience's "from
    -- your hand"; Nothing where the sentence names none (World War Hulk's
    -- chapter I).
    from :: Maybe Zone.Zone,
    -- | Weftwalking's "the first spell each player casts during each of their
    -- turns", read as Parun.Types.CostModifier.onlyNth is, over the casts
    -- matching `spells`; Nothing reaches every matching cast.
    onlyNth :: Maybe TurnOrdinal.TurnOrdinal,
    -- | Warped Space's "Once each turn", spent only by a cast that pays this
    -- grant's cost, where `onlyNth` counts every matching cast; the budget
    -- Parun.Types.CastFromZone's `limit` is.
    limit :: PermissionLimit.PermissionLimit
  }
  deriving (Eq, Ord, Show)
