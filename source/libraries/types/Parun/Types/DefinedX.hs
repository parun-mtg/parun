module Parun.Types.DefinedX where

import qualified Parun.Types.ExiledPile as ExiledPile
import qualified Parun.Types.Quantity as Quantity

-- | CR 107.3c: the value of X an activated ability's own text defines, so its
-- activator announces none (CR 107.3a).
data DefinedX = MkDefinedX
  { -- | The value -- Soul Foundry's "X is the mana value of that card".
    quantity :: Quantity.Quantity,
    -- | CR 601.2b / 607.3: the pile of the source whose one card the activator
    -- chooses as X would be announced, where that choice defines X (Caller of
    -- the Untamed), bound under Parun.Engine.Binding.definingCard for
    -- `quantity` and the effect to read. Nothing for an X no choice defines.
    choice :: Maybe ExiledPile.ExiledPile
  }
  deriving (Eq, Ord, Show)
