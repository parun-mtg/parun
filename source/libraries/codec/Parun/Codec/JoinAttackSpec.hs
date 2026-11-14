module Parun.Codec.JoinAttackSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.JoinAttack as JoinAttack
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.JoinAttack as JoinAttack
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.JoinAttack" $ do
  Spec.it s "round-trips" $
    Common.assertCodec
      s
      JoinAttack.codec
      (JoinAttack.MkJoinAttack (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))) (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatAttackingPlayer"))))
      " {\"alongside\":{\"type\":\"InSlot\",\"value\":\"thatAttackingPlayer\"},\"creature\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s JoinAttack.codec
