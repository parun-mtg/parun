module Parun.Codec.AnotherCounterSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.AnotherCounter as AnotherCounter
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AnotherCounter as AnotherCounter
import qualified Parun.Types.EachKind as EachKind
import qualified Parun.Types.KindStep as KindStep
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AnotherCounter" $ do
  Spec.it s "EachKind carries its slot and step" $
    Common.assertCodec s AnotherCounter.codec (AnotherCounter.EachKind (EachKind.MkEachKind (SlotName.MkSlotName (Text.pack "target")) KindStep.Another)) " {\"type\":\"EachKind\",\"value\":{\"slot\":\"target\",\"step\":\"Another\"}} "
  Spec.it s "ChosenKind carries its slot" $
    Common.assertCodec s AnotherCounter.codec (AnotherCounter.ChosenKind (SlotName.MkSlotName (Text.pack "target"))) " {\"type\":\"ChosenKind\",\"value\":\"target\"} "
