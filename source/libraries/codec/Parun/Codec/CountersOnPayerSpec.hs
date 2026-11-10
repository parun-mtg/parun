module Parun.Codec.CountersOnPayerSpec where

import qualified Parun.Codec.CountersOnPayer as CountersOnPayer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CountersOnPayer as CountersOnPayer
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CountersOnPayer" $ do
  -- The Serpent Society's five poison counters.
  Spec.it s "MkCountersOnPayer" $
    Common.assertCodec
      s
      CountersOnPayer.codec
      ( CountersOnPayer.MkCountersOnPayer
          { CountersOnPayer.kind = PlayerCounterKind.Poison,
            CountersOnPayer.count = 5
          }
      )
      " {\"count\":5,\"kind\":\"Poison\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CountersOnPayer.codec
