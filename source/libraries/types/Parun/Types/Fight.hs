module Parun.Types.Fight where

import qualified Parun.Types.Hop as Hop
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's Fight arm.
--
-- TWO SlotNames and no ObjectRef, because CR 701.14a's subject is a pair:
-- "a spell or ability may instruct a creature to fight ANOTHER creature or it
-- may instruct TWO creatures to fight each other". Both halves of that sentence
-- are two slots; the first is Prey Upon's, and the second is Arena's "those
-- creatures fight each other". Several pairs at once are `paired`'s. A slot need not be a TARGET slot:
-- Nightfall Predator's "this creature fights target creature" names CR 113.7's
-- reserved `self` in one and a target in the other, which is also how the two
-- can name ONE permanent (CR 701.14c).
--
-- Not an ordered pair in any rules sense -- CR 701.14a's damage is simultaneous
-- and symmetric -- so `first` and `second` name the card's own two clauses rather
-- than a sequence.
data Fight = MkFight
  { first :: SlotName.SlotName,
    second :: SlotName.SlotName,
    -- | CR 301.5a / 303.4b / 608.2h: which object `first` names, its slot's own
    -- or the permanent that one is attached to as the fight happens (Novel
    -- Nunchaku's "equipped creature fights"). The codec omits
    -- Hop.Itself.
    hop :: Hop.Hop,
    -- | Where to record the excess damage (CR 120.10) dealt to `second`'s
    -- creature: Parun.Types.DealDamage's `excessTally`, for a fight.
    -- data/scenarios/the-last-agni-kai-adds-red-mana-for-the-excess-and-keeps-it.json
    -- proves it.
    excessTally :: Maybe SlotName.SlotName,
    -- | CR 701.14a / 608.2d: each slot names a group, and the resolving
    -- controller pairs each member of the smaller with a different member of
    -- the other, every pair fighting at once (Ezuri's Predation's "each of
    -- those tokens fights a different one of those creatures"). The codec
    -- omits False. data/scenarios/ezuris-predation-pairs-each-beast-with-a-different-opponents-creature.json
    -- proves it, and the Doubling Season scenario beside it the extras.
    paired :: Bool
  }
  deriving (Eq, Ord, Show)
