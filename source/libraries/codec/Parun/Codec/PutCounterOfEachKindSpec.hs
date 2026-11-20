module Parun.Codec.PutCounterOfEachKindSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.PutCounterOfEachKind as PutCounterOfEachKind
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PutCounterOfEachKind as PutCounterOfEachKind
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PutCounterOfEachKind" $ do
  Spec.it s "MkPutCounterOfEachKind" $
    Common.assertCodec
      s
      PutCounterOfEachKind.codec
      ( PutCounterOfEachKind.MkPutCounterOfEachKind
          { PutCounterOfEachKind.among = ObjectRef.EachMatching (Filter.And []),
            PutCounterOfEachKind.ref = ObjectRef.OneAmong CardSelection.ByChoice (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "fish")))
          }
      )
      " {\"among\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"And\",\"value\":[]}},\"ref\":{\"type\":\"OneChosenAmong\",\"value\":{\"type\":\"InSlot\",\"value\":\"fish\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PutCounterOfEachKind.codec
