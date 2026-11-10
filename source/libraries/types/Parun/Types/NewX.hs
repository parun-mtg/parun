module Parun.Types.NewX where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Quantity as Quantity

-- | What Parun.Types.SetX makes an announced X (CR 107.3a).
data NewX
  = -- | This quantity: Glava, Five-Advents Mage's "the value of X become 5".
    Becomes Quantity.Quantity
  | -- | The object's X as the instruction applies, times this: Unbound
    -- Flourishing's "double the value of X", which two Flourishings make four
    -- times, as its ruling has it.
    Multiplied Natural.Natural
  deriving (Eq, Ord, Show)

-- | The quantities a NewX carries, for the walkers that read a payload's
-- slots.
quantities :: NewX -> [Quantity.Quantity]
quantities newX = case newX of
  Becomes quantity -> [quantity]
  Multiplied _ -> []
