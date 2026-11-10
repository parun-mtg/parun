module Parun.Codec.CardTallySpec where

import qualified Parun.Codec.CardTally as CardTally
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.Color as Color

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CardTally" $ do
  Spec.it s "ManaValue" $
    Common.assertCodec
      s
      CardTally.codec
      CardTally.ManaValue
      " {\"type\":\"ManaValue\"} "
  Spec.it s "ManaSymbolsOf" $
    Common.assertCodec
      s
      CardTally.codec
      (CardTally.ManaSymbolsOf Color.Black)
      " {\"type\":\"ManaSymbolsOf\",\"value\":\"Black\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CardTally.codec
