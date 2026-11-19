module Parun.Codec.MeldSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Meld as Meld
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EntryAttack as EntryAttack
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Meld as Meld
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TapState as TapState

-- A card codec that stands in for Parun.Codec.Card's, which this sublibrary's
-- other parametric specs also decline to drag in for a payload they never look
-- inside. Text is enough to tell "the combined face came through the supplied
-- codec" from "it came from somewhere else".
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Meld" $ do
  -- CR 701.42a's two halves: which cards are melded -- the slot Hanweir
  -- Battlements' own exile bound (CR 400.7j) -- and the combined back face the
  -- ability carries inline.
  Spec.it s "the cards to meld, and the combined back face" $
    Common.assertCodec
      s
      (Meld.codec cardCodec Common.text)
      Meld.MkMeld
        { Meld.objects = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "melding")),
          Meld.result = Text.pack "Hanweir, the Writhing Township",
          Meld.riders = EntryRiders.defaultValue
        }
      " {\"objects\":{\"type\":\"InSlot\",\"value\":\"melding\"},\"result\":\"Hanweir, the Writhing Township\"} "
  -- Mishra, Claimed by Gix's "It enters tapped and attacking" (CR 110.5b, CR
  -- 508.4).
  Spec.it s "the riders the melded permanent enters with" $
    Common.assertCodec
      s
      (Meld.codec cardCodec Common.text)
      Meld.MkMeld
        { Meld.objects = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "melding")),
          Meld.result = Text.pack "Mishra, Lost to Phyrexia",
          Meld.riders = EntryRiders.defaultValue {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = Just EntryAttack.Chosen}
        }
      " {\"objects\":{\"type\":\"InSlot\",\"value\":\"melding\"},\"result\":\"Mishra, Lost to Phyrexia\",\"riders\":{\"tapped\":\"Tapped\",\"attacking\":{\"type\":\"Chosen\"}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s (Meld.codec cardCodec Common.text)
