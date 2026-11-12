module Parun.Codec.CounterLossRSpec where

import qualified Parun.Codec.CounterLossR as CounterLossR
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterLossR as CounterLossR
import qualified Parun.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CounterLossR" $ do
  -- CR 614.1a / 120.3c: Deification.
  Spec.it s "MkCounterLossR" $
    Common.assertCodec
      s
      CounterLossR.codec
      CounterLossR.MkCounterLossR
        { CounterLossR.onWhat = Filter.HasCardType CardType.Planeswalker,
          CounterLossR.whichKind = CounterKind.Loyalty,
          CounterLossR.leaveAtLeast = 1
        }
      " {\"onWhat\":{\"type\":\"HasCardType\",\"value\":\"Planeswalker\"},\"whichKind\":{\"type\":\"Loyalty\"},\"leaveAtLeast\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterLossR.codec
