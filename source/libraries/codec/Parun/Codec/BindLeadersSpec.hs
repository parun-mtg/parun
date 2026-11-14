module Parun.Codec.BindLeadersSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.BindLeaders as BindLeaders
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.BindLeaders as BindLeaders
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BindLeaders" $ do
  Spec.it s "round-trips" $
    Common.assertCodec
      s
      BindLeaders.codec
      (BindLeaders.MkBindLeaders {BindLeaders.among = SlotName.MkSlotName (Text.pack "exiled"), BindLeaders.measure = Measure.ManaValue, BindLeaders.slot = SlotName.MkSlotName (Text.pack "leaders")})
      " {\"among\":\"exiled\",\"measure\":\"ManaValue\",\"slot\":\"leaders\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BindLeaders.codec
