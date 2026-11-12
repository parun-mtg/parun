module Parun.Codec.CountersOnThisSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.CountersOnThis as CountersOnThis
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterName as CounterName
import qualified Parun.Types.CountersOnThis as CountersOnThis
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.SlotName as SlotName

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (CountersOnThis.CountersOnThis Keyword.Keyword)
codec = CountersOnThis.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CountersOnThis" $ do
  -- Hickory Woodlot's one depletion counter.
  Spec.it s "MkCountersOnThis" $
    Common.assertCodec
      s
      codec
      ( CountersOnThis.MkCountersOnThis
          { CountersOnThis.kind = CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "depletion")),
            CountersOnThis.count = CostAmount.Fixed 1,
            CountersOnThis.holder = Nothing
          }
      )
      " {\"count\":{\"type\":\"Fixed\",\"value\":1},\"kind\":{\"type\":\"Named\",\"value\":\"depletion\"}} "
  -- Warden of the Grove's endure X on the creature that entered.
  Spec.it s "MkCountersOnThis with a holder" $
    Common.assertCodec
      s
      codec
      ( CountersOnThis.MkCountersOnThis
          { CountersOnThis.kind = CounterKind.PlusOnePlusOne,
            CountersOnThis.count = CostAmount.Announced (SlotName.MkSlotName (Text.pack "X")),
            CountersOnThis.holder = Just (SlotName.MkSlotName (Text.pack "became"))
          }
      )
      " {\"count\":{\"type\":\"Announced\",\"value\":\"X\"},\"holder\":\"became\",\"kind\":{\"type\":\"PlusOnePlusOne\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
