module Parun.Codec.ReselectAttackSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.ReselectAttack as ReselectAttack
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.ReselectAttack as ReselectAttack
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ReselectAttack" $ do
  Spec.it s "round-trips" $
    Common.assertCodec
      s
      ReselectAttack.codec
      (ReselectAttack.MkReselectAttack (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) (Set.fromList [AttackTargetKind.OfPlayer, AttackTargetKind.OfBattle]))
      " {\"creature\":{\"type\":\"InSlot\",\"value\":\"target\"},\"kinds\":[\"OfPlayer\",\"OfBattle\"]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ReselectAttack.codec
