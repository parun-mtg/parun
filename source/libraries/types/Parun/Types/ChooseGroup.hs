module Parun.Types.ChooseGroup where

import qualified Parun.Types.Grouping as Grouping
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's ChooseGroup arm: a player chooses one
-- of the groups, and its members are bound as a group into the slot.
data ChooseGroup = MkChooseGroup
  { among :: Grouping.Grouping,
    slot :: SlotName.SlotName,
    -- | The object the choice is made for, which the prompt names in place of
    -- the source: Raging River's attacking creature. Elided rather than
    -- written null.
    about :: Maybe ObjectRef.ObjectRef,
    -- | CR 608.2d: who chooses, You by default; Hostile Negotiations' "an
    -- opponent chooses one of those piles". Elided when You.
    chooser :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
