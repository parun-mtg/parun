module Parun.Codec.ControlPlayerNextSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ControlPlayerNext as ControlPlayerNext
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ControlPlayerNext as ControlPlayerNext
import qualified Parun.Types.ControlSpan as ControlSpan
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ControlPlayerNext" $ do
  Spec.it s "MkControlPlayerNext" $
    Common.assertCodec
      s
      ControlPlayerNext.codec
      ( ControlPlayerNext.MkControlPlayerNext
          { ControlPlayerNext.slot = SlotName.MkSlotName (Text.pack "target"),
            ControlPlayerNext.during = ControlSpan.NextCombatPhase
          }
      )
      " {\"slot\":\"target\",\"during\":\"NextCombatPhase\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlPlayerNext.codec
