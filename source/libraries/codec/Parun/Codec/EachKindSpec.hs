module Parun.Codec.EachKindSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.EachKind as EachKind
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EachKind as EachKind
import qualified Parun.Types.KindStep as KindStep
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.EachKind" $ do
  Spec.it s "MkEachKind" $
    Common.assertCodec
      s
      EachKind.codec
      (EachKind.MkEachKind (SlotName.MkSlotName (Text.pack "target")) KindStep.AnotherOrRemoveOne)
      " {\"slot\":\"target\",\"step\":\"AnotherOrRemoveOne\"} "

  Spec.it s "has a schema" $ Common.assertHasSchema s EachKind.codec
