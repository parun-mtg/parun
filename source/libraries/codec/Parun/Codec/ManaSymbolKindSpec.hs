module Parun.Codec.ManaSymbolKindSpec where

import qualified Parun.Codec.ManaSymbolKind as ManaSymbolKind
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ManaSymbolKind as ManaSymbolKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaSymbolKind" $ do
  Spec.it s "VariableSymbol" $
    Common.assertCodec
      s
      ManaSymbolKind.codec
      ManaSymbolKind.VariableSymbol
      " \"VariableSymbol\" "
  Spec.it s "PhyrexianSymbol" $
    Common.assertCodec
      s
      ManaSymbolKind.codec
      ManaSymbolKind.PhyrexianSymbol
      " \"PhyrexianSymbol\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ManaSymbolKind.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaSymbolKind.codec
