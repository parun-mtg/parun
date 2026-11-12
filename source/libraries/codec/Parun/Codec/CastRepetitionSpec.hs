module Parun.Codec.CastRepetitionSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.CastRepetition as CastRepetition
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CastRepetition as CastRepetition
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TotalCap as TotalCap

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CastRepetition" $ do
  Spec.it s "Once" $
    Common.assertCodec
      s
      CastRepetition.codec
      CastRepetition.Once
      " {\"type\":\"Once\"} "
  Spec.it s "AnyNumber" $
    Common.assertCodec
      s
      CastRepetition.codec
      CastRepetition.AnyNumber
      " {\"type\":\"AnyNumber\"} "
  Spec.it s "WithinTotal" $
    Common.assertCodec
      s
      CastRepetition.codec
      (CastRepetition.WithinTotal TotalCap.MkTotalCap {TotalCap.weight = Quantity.Measure Measure.ManaValue, TotalCap.most = Quantity.InSlot (SlotName.MkSlotName (Text.pack "X"))})
      " {\"type\":\"WithinTotal\",\"value\":{\"most\":{\"type\":\"InSlot\",\"value\":\"X\"},\"weight\":{\"type\":\"Measure\",\"value\":\"ManaValue\"}}} "
  Spec.it s "UpTo" $
    Common.assertCodec
      s
      CastRepetition.codec
      (CastRepetition.UpTo (Quantity.InSlot (SlotName.MkSlotName (Text.pack "casts"))))
      " {\"type\":\"UpTo\",\"value\":{\"type\":\"InSlot\",\"value\":\"casts\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastRepetition.codec
