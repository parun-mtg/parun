module Parun.Codec.NextWindowOfSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.NextWindowOf as NextWindowOf
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.Hop as Hop
import qualified Parun.Types.NextWindow as NextWindow
import qualified Parun.Types.NextWindowOf as NextWindowOf.Type
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.NextWindowOf" $ do
  Spec.it s "names the window and the player" $
    Common.assertCodec
      s
      NextWindowOf.codec
      (NextWindowOf.Type.MkNextWindowOf NextWindow.CombatPhase (PlayerRef.HolderOfBound Holder.Controller Hop.Itself (SlotName.MkSlotName (Text.pack "target"))))
      " {\"player\":{\"type\":\"HolderOfBound\",\"value\":{\"holder\":\"Controller\",\"hop\":\"Itself\",\"slot\":\"target\"}},\"window\":\"CombatPhase\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s NextWindowOf.codec
