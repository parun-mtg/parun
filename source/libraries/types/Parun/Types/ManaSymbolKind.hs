module Parun.Types.ManaSymbolKind where

import qualified Parun.Types.Color as Color

-- | Which kind of mana symbol Filter.ManaCostHasSymbol looks for in a mana or
-- activation cost, and Quantity.ManaSymbolCount counts.
data ManaSymbolKind
  = -- | CR 107.3: {X} (Zaxara, the Exemplary; Gaddock Teeg).
    VariableSymbol
  | -- | CR 107.4g: {H}, any of the fifteen Phyrexian mana symbols (Rage Extractor).
    PhyrexianSymbol
  | -- | CR 107.4e / 107.4f: a mana symbol of that colour, hybrid and Phyrexian
    -- included (Namor the Sub-Mariner).
    ColoredSymbol Color.Color
  deriving (Eq, Ord, Show)
