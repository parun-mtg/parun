module Parun.Codec.PlayAsCopySpec where

import qualified Data.Text as Text
import qualified Parun.Codec.PlayAsCopy as PlayAsCopy
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayAsCopy as PlayAsCopy
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayAsCopy" $ do
  Spec.it s "MkPlayAsCopy" $
    Common.assertCodec
      s
      PlayAsCopy.codec
      (PlayAsCopy.MkPlayAsCopy (Filter.HasCardType CardType.Instant) (Filter.ProducesColorOf (SlotName.MkSlotName (Text.pack "thatPlayedLand"))))
      " {\"cards\":{\"type\":\"HasCardType\",\"value\":\"Instant\"},\"original\":{\"type\":\"ProducesColorOf\",\"value\":\"thatPlayedLand\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayAsCopy.codec
