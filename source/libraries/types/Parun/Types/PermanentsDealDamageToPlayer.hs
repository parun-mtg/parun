module Parun.Types.PermanentsDealDamageToPlayer where

import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 603.2c: which permanents' damage fires the batch ability, of which
-- kind, and which player it has to have been dealt to -- Norn's Decree's "one
-- or more creatures an opponent controls deal combat damage to you", The Thing,
-- Ben Grimm's "one or more Heroes you control deal damage to a player".
--
-- A record because the printed form names two subjects, the damagers and the damaged player, and the two are read from
-- the same CR 109.5 perspective.
data PermanentsDealDamageToPlayer = MkPermanentsDealDamageToPlayer
  { filter :: Filter.Filter Keyword.Keyword,
    -- | Nothing for damage of any kind (CR 120.2), DamageTrigger.kind's reading.
    kind :: Maybe DamageKind.DamageKind,
    recipient :: PlayerRelation.PlayerRelation,
    -- | CR 603.2c: "to ONE OR MORE PLAYERS" (Forth Eorlingas!), one occurrence
    -- however many players the step damaged, where False is "to a player", one
    -- occurrence per damaged player.
    oneOrMorePlayers :: Bool
  }
  deriving (Eq, Ord, Show)
