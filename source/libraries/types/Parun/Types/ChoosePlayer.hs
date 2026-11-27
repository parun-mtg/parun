module Parun.Types.ChoosePlayer where

import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.SlotName as SlotName

-- | CR 608.2d: the payload of Parun.Types.Effect's ChoosePlayer arm -- which
-- players a resolving effect offers, and where the answer is bound.
--
-- The SCOPE is what separates Skullwinder's "choose an opponent"
-- (PlayerScope.Related Opponent) from Stadium Vendors' "choose a player"
-- (PlayerScope.Related AnyPlayer, which includes the chooser). It is resolved through
-- Parun.Engine.PlayerEffect.playersInScope, the one fold every player-set read
-- shares, against CR 109.5's "you" -- the resolving controller.
data ChoosePlayer = MkChoosePlayer
  { scope :: PlayerScope.PlayerScope,
    slot :: SlotName.SlotName,
    -- | Players a slot names that are not offered -- Dack Fayden, Helping
    -- Hand's "for each of those permanents, choose a different opponent",
    -- over the loop's Parun.Engine.Binding.chosenForOthers. A slot naming
    -- nobody excludes nobody, as PlayerRef.EachPlayerExcept's does.
    excluding :: Maybe SlotName.SlotName,
    -- | Whether only players the effect's source could become attached to are
    -- offered -- Curse of Leeches' "attach it to a player" (CR 701.3a, 608.2d).
    attaching :: Bool,
    -- | CR 607.2d: whether the choice is also written to the source as its
    -- PlayerChoice.ChosenPlayer, which its other abilities read as "the chosen
    -- player" (Imprison This Insolent Wretch).
    linked :: Bool,
    -- | CR 608.2d: who chooses, You by default; Choose Your Champion's "target
    -- opponent chooses a player". The scope is still read against "you".
    chooser :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
