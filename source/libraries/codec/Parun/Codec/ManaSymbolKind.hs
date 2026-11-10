module Parun.Codec.ManaSymbolKind where

import qualified Parun.Codec.Color as Color
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ManaSymbolKind as ManaSymbolKind

-- | Tagged rather than an enum, since one arm carries a colour.
codec :: Codec.Codec ManaSymbolKind.ManaSymbolKind
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "VariableSymbol" ManaSymbolKind.VariableSymbol,
      Arm.nullary "PhyrexianSymbol" ManaSymbolKind.PhyrexianSymbol,
      Arm.payload "ColoredSymbol" Color.codec ManaSymbolKind.ColoredSymbol (\x -> case x of ManaSymbolKind.ColoredSymbol y -> Just y; _ -> Nothing)
    ]

tagOf :: ManaSymbolKind.ManaSymbolKind -> String
tagOf x = case x of
  ManaSymbolKind.VariableSymbol {} -> "VariableSymbol"
  ManaSymbolKind.PhyrexianSymbol {} -> "PhyrexianSymbol"
  ManaSymbolKind.ColoredSymbol {} -> "ColoredSymbol"
