module Parun.Codec.FromAnywhereSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.FromAnywhere as FromAnywhere
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.FromAnywhere as FromAnywhere
import qualified Parun.Types.Reach as Reach
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.FromAnywhere" $ do
  Spec.it s "MkFromAnywhere, no among" $
    Common.assertCodec
      s
      FromAnywhere.codec
      (FromAnywhere.MkFromAnywhere {FromAnywhere.slot = SlotName.MkSlotName (Text.pack "self"), FromAnywhere.among = Nothing, FromAnywhere.reach = Reach.PublicZone})
      " {\"slot\":\"self\"} "
  Spec.it s "MkFromAnywhere, every key" $
    Common.assertCodec
      s
      FromAnywhere.codec
      (FromAnywhere.MkFromAnywhere {FromAnywhere.slot = SlotName.MkSlotName (Text.pack "self"), FromAnywhere.among = Just (SlotName.MkSlotName (Text.pack "exiled")), FromAnywhere.reach = Reach.AnyZone})
      " {\"slot\":\"self\",\"among\":\"exiled\",\"reach\":\"AnyZone\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s FromAnywhere.codec
