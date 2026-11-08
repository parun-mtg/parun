module Parun.Codec.DeckLimitSpec where

import qualified Parun.Codec.DeckLimit as DeckLimit
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.DeckLimit as DeckLimit

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DeckLimit" $ do
  -- CR 100.2a: Nazgûl's "up to nine".
  Spec.it s "UpTo" $
    Common.assertCodec s DeckLimit.codec (DeckLimit.UpTo 9) " {\"type\":\"UpTo\",\"value\":9} "
  -- CR 100.2a: Relentless Rats' "any number".
  Spec.it s "AnyNumber" $
    Common.assertCodec s DeckLimit.codec DeckLimit.AnyNumber " {\"type\":\"AnyNumber\"} "
