module Parun.Codec.ArrangeInLibrarySpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ArrangeInLibrary as ArrangeInLibrary
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ArrangeInLibrary as ArrangeInLibrary
import qualified Parun.Types.CardSelection as CardSelection
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ArrangeInLibrary" $ do
  -- Ponder's "then put them back in any order".
  Spec.it s "MkArrangeInLibrary" $
    Common.assertCodec
      s
      ArrangeInLibrary.codec
      (ArrangeInLibrary.MkArrangeInLibrary (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "looked"))) (PlayerRef.Relative PlayerRelation.You) Nothing Nothing CardSelection.ByChoice)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"looked\"}} "
  -- Tahngarth's Glare's "that player ... puts them back in any order".
  Spec.it s "MkArrangeInLibrary, a named chooser" $
    Common.assertCodec
      s
      ArrangeInLibrary.codec
      (ArrangeInLibrary.MkArrangeInLibrary (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "looked"))) (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) Nothing Nothing CardSelection.ByChoice)
      " {\"chooser\":{\"type\":\"InSlot\",\"value\":\"target\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"looked\"}} "
  -- Pools of Becoming's "put the revealed cards on the bottom of your planar
  -- deck in any order".
  Spec.it s "MkArrangeInLibrary, a stated end" $
    Common.assertCodec
      s
      ArrangeInLibrary.codec
      (ArrangeInLibrary.MkArrangeInLibrary (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "revealed"))) (PlayerRef.Relative PlayerRelation.You) (Just LibraryPosition.Bottom) Nothing CardSelection.ByChoice)
      " {\"end\":\"Bottom\",\"ref\":{\"type\":\"InSlot\",\"value\":\"revealed\"}} "
  -- Interplanar Tunnel's "put the rest of the revealed cards on the bottom in
  -- a random order", and the plane put on top bound for that "rest".
  Spec.it s "MkArrangeInLibrary, a bound slot and a random order" $
    Common.assertCodec
      s
      ArrangeInLibrary.codec
      (ArrangeInLibrary.MkArrangeInLibrary (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "revealed"))) (PlayerRef.Relative PlayerRelation.You) (Just LibraryPosition.Bottom) (Just (SlotName.MkSlotName (Text.pack "top"))) CardSelection.AtRandom)
      " {\"end\":\"Bottom\",\"ref\":{\"type\":\"InSlot\",\"value\":\"revealed\"},\"selection\":\"AtRandom\",\"slot\":\"top\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ArrangeInLibrary.codec
