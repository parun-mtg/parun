module Parun.Types.FreeCast where

import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TurnOrdinal as TurnOrdinal
import qualified Parun.Types.Zone as Zone

-- | The payload of Parun.Types.PlayerEffect's PayRatherThanManaCost arm:
-- which spells CR 118.9's grant covers, what it has their caster pay instead,
-- the number their filter's EnclosingAmount operand compares against, the zone
-- it is confined to, and which cast of a turn it reaches.
data FreeCast = MkFreeCast
  { -- | Read off the card's CR 613.1 view where it is cast from, as
    -- Parun.Types.CastFromZone's `matching` is.
    spells :: Filter.Filter Keyword.Keyword,
    -- | CR 118.9: the alternative cost paid in place of the mana cost, read
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
    onlyNth :: Maybe TurnOrdinal.TurnOrdinal
  }
  deriving (Eq, Ord, Show)
