module Parun.Codec.LifeExchangeSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.LifeExchange as LifeExchange
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ExchangeSides as ExchangeSides
import qualified Parun.Types.LifeExchange as LifeExchange
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LifeExchange" $ do
  Spec.it s "an exchange without a tally omits it" $
    Common.assertCodec
      s
      LifeExchange.codec
      LifeExchange.MkLifeExchange
        { LifeExchange.sides = ExchangeSides.WithController (SlotName.MkSlotName (Text.pack "target")),
          LifeExchange.tally = Nothing
        }
      " {\"sides\":{\"type\":\"WithController\",\"value\":\"target\"}} "
  Spec.it s "an exchange names a tally slot" $
    Common.assertCodec
      s
      LifeExchange.codec
      LifeExchange.MkLifeExchange
        { LifeExchange.sides = ExchangeSides.WithController (SlotName.MkSlotName (Text.pack "target")),
          LifeExchange.tally = Just (SlotName.MkSlotName (Text.pack "lost"))
        }
      " {\"sides\":{\"type\":\"WithController\",\"value\":\"target\"},\"tally\":\"lost\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeExchange.codec
