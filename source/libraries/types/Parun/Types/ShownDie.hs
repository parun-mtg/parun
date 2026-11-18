module Parun.Types.ShownDie where

import qualified Numeric.Natural as Natural

-- | A die a permanent IS, as it lies: the kind of die, which CR 706.1a's N
-- describes whole, and the result it shows -- Pippa, Duchess of Dice's Die
-- creature token, whose rulings make the die that represents it rerollable at
-- any time. `result` is a Quantity on Parun.Types.Create, settled to a number
-- on Parun.Types.Object.
data ShownDie result = MkShownDie
  { sides :: Natural.Natural,
    result :: result
  }
  deriving (Eq, Ord, Show)
