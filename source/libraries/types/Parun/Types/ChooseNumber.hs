module Parun.Types.ChooseNumber where

import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's ChooseNumber arm: where the number is
-- bound, and the most it may be, read as the choice is asked (CR 608.2h) --
-- Trade Secrets' "draw up to four cards", Clockwork Beast's "up to X" -- beside
-- Rites of Initiation's unbounded "any number" (CR 107.1c).
--
-- The CHOOSER is a PlayerRef naming ONE seat, Parun.Types.ChosenPermanents'
-- reading: CR 608.2c's default is the resolving controller, @Relative You@, and
-- Temporary Truce's "each player may draw up to two cards" is a CR 608.2f loop
-- over the players whose body has the member choose, @InSlot@ the loop's slot.
-- A ref naming no seat names no number, and the instruction is ignored (CR
-- 101.3).
data ChooseNumber = MkChooseNumber
  { slot :: SlotName.SlotName,
    upTo :: Maybe Quantity.Quantity,
    chooser :: PlayerRef.PlayerRef,
    -- | Whether the chooser picks the number or randomness does, each number
    -- from zero to the bound equally likely (Jaya Ballard Avatar's "an amount
    -- of damage chosen at random from 0 to X"); at random, nobody chooses and
    -- the chooser is unread.
    --
    -- Unprinted: a random number with no bound, or with a chooser; Parun.FilterPositionLintSpec's
    -- "no card writes a random ChooseNumber with no bound or a chooser" keeps
    -- both out of card data.
    selection :: CardSelection.CardSelection
  }
  deriving (Eq, Ord, Show)
