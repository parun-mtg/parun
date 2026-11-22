module Parun.Codec.PutCountersSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.PutCounters as PutCounters
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PutCounters as PutCounters
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PutCounters" $ do
  -- An ObjectRef and not a bare slot, which is what lets Renegade Krasis' swept
  -- set be written (CR 115.10a).
  Spec.it s "MkPutCounters, all three keys" $
    Common.assertCodec
      s
      PutCounters.codec
      ( PutCounters.MkPutCounters
          { PutCounters.kind = CounterKind.PlusOnePlusOne,
            PutCounters.alternatives = [],
            PutCounters.quantity = Quantity.Literal 1,
            PutCounters.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PutCounters.totalAtMost = Nothing,
            PutCounters.slot = Nothing,
            PutCounters.putter = Nothing
          }
      )
      " {\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "MkPutCounters, a limit on the total" $
    Common.assertCodec
      s
      PutCounters.codec
      ( PutCounters.MkPutCounters
          { PutCounters.kind = CounterKind.PlusOnePlusOne,
            PutCounters.alternatives = [],
            PutCounters.quantity = Quantity.Literal 1,
            PutCounters.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PutCounters.totalAtMost = Just 7,
            PutCounters.slot = Nothing,
            PutCounters.putter = Nothing
          }
      )
      " {\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"totalAtMost\":7} "
  Spec.it s "MkPutCounters, the placed permanents bound" $
    Common.assertCodec
      s
      PutCounters.codec
      ( PutCounters.MkPutCounters
          { PutCounters.kind = CounterKind.PlusOnePlusOne,
            PutCounters.alternatives = [],
            PutCounters.quantity = Quantity.Literal 1,
            PutCounters.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PutCounters.totalAtMost = Nothing,
            PutCounters.slot = Just (SlotName.MkSlotName (Text.pack "placed")),
            PutCounters.putter = Nothing
          }
      )
      " {\"kind\":{\"type\":\"PlusOnePlusOne\"},\"quantity\":{\"type\":\"Literal\",\"value\":1},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"slot\":\"placed\"} "
  Spec.it s "MkPutCounters, a choice of kinds" $
    Common.assertCodec
      s
      PutCounters.codec
      ( PutCounters.MkPutCounters
          { PutCounters.kind = CounterKind.Keyword Keyword.Reach,
            PutCounters.alternatives = [CounterKind.Keyword Keyword.Vigilance, CounterKind.Keyword Keyword.Trample],
            PutCounters.quantity = Quantity.Literal 1,
            PutCounters.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "token")),
            PutCounters.totalAtMost = Nothing,
            PutCounters.slot = Nothing,
            PutCounters.putter = Nothing
          }
      )
      " {\"kind\":{\"type\":\"Keyword\",\"value\":{\"type\":\"Reach\"}},\"alternatives\":[{\"type\":\"Keyword\",\"value\":{\"type\":\"Vigilance\"}},{\"type\":\"Keyword\",\"value\":{\"type\":\"Trample\"}}],\"quantity\":{\"type\":\"Literal\",\"value\":1},\"ref\":{\"type\":\"InSlot\",\"value\":\"token\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PutCounters.codec
