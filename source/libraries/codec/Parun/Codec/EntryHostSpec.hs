module Parun.Codec.EntryHostSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.EntryHost as EntryHost
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.EntryHost as EntryHost
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.EntryHost" $ do
  Spec.it s "Specified" $
    Common.assertCodec
      s
      EntryHost.codec
      (EntryHost.Specified (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"Specified\",\"value\":\"target\"} "
  Spec.it s "Chosen" $
    Common.assertCodec
      s
      EntryHost.codec
      (EntryHost.Chosen CardSelection.ByChoice Nothing Filter.IsSource)
      " {\"type\":\"Chosen\",\"value\":{\"filter\":{\"type\":\"IsSource\"}}} "
  Spec.it s "Chosen at random" $
    Common.assertCodec
      s
      EntryHost.codec
      (EntryHost.Chosen CardSelection.AtRandom Nothing Filter.IsSource)
      " {\"type\":\"Chosen\",\"value\":{\"filter\":{\"type\":\"IsSource\"},\"selection\":\"AtRandom\"}} "
  Spec.it s "Chosen by another player" $
    Common.assertCodec
      s
      EntryHost.codec
      (EntryHost.Chosen CardSelection.ByChoice (Just (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")))) Filter.IsSource)
      " {\"type\":\"Chosen\",\"value\":{\"chooser\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"filter\":{\"type\":\"IsSource\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EntryHost.codec
