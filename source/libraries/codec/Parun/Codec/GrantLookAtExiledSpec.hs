module Parun.Codec.GrantLookAtExiledSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.GrantLookAtExiled as GrantLookAtExiled
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.GrantLookAtExiled as GrantLookAtExiled
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.Hop as Hop
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.GrantLookAtExiled" $ do
  -- CR 406.3 alone (Extract Power), which is what a card file that omits the
  -- rider gets.
  Spec.it s "the cards, with CR 702.75a's rider absent" $
    Common.assertCodec
      s
      GrantLookAtExiled.codec
      GrantLookAtExiled.MkGrantLookAtExiled
        { GrantLookAtExiled.cards = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
          GrantLookAtExiled.player = PlayerRef.Relative PlayerRelation.You,
          GrantLookAtExiled.followsExiler = False
        }
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"exiled\"}} "
  -- CR 702.75a's rider, which hideaway mints.
  Spec.it s "the rider round-trips when it is set" $
    Common.assertCodec
      s
      GrantLookAtExiled.codec
      GrantLookAtExiled.MkGrantLookAtExiled
        { GrantLookAtExiled.cards = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "hidden")),
          GrantLookAtExiled.player = PlayerRef.Relative PlayerRelation.You,
          GrantLookAtExiled.followsExiler = True
        }
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"hidden\"},\"followsExiler\":true} "
  -- CR 406.3's looker other than "you": Gonti, Night Minister's "its
  -- controller".
  Spec.it s "a looker other than the resolving controller round-trips" $
    Common.assertCodec
      s
      GrantLookAtExiled.codec
      GrantLookAtExiled.MkGrantLookAtExiled
        { GrantLookAtExiled.cards = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
          GrantLookAtExiled.player = PlayerRef.HolderOfBound Holder.Controller Hop.Itself (SlotName.MkSlotName (Text.pack "thatDamager")),
          GrantLookAtExiled.followsExiler = False
        }
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"player\":{\"type\":\"HolderOfBound\",\"value\":{\"holder\":\"Controller\",\"hop\":\"Itself\",\"slot\":\"thatDamager\"}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s GrantLookAtExiled.codec
