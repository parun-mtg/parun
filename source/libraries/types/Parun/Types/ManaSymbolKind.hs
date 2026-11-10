module Parun.Types.ManaSymbolKind where

-- | Which kind of mana symbol Filter.ManaCostHasSymbol looks for in a mana or
-- activation cost.
data ManaSymbolKind
  = -- | CR 107.3: {X} (Zaxara, the Exemplary; Gaddock Teeg).
    VariableSymbol
  | -- | CR 107.4g: {H}, any of the fifteen Phyrexian mana symbols (Rage Extractor).
    PhyrexianSymbol
  deriving (Bounded, Enum, Eq, Ord, Show)
