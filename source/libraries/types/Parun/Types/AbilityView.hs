module Parun.Types.AbilityView where

import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.StackedAbility as StackedAbility

-- | CR 113.7a / 602.2a: an ability on the stack as one seat sees it.
data AbilityView = MkAbilityView
  { -- | The object it came from where the seat may know that object, and
    -- Nothing for an inherent trigger (CR 726.2) or a source in a zone hidden
    -- from the seat.
    source :: Maybe ObjectId.ObjectId,
    -- | Which ability it is, which the stack shows every seat (CR 400.2).
    ability :: StackedAbility.StackedAbility
  }
  deriving (Eq, Show)
