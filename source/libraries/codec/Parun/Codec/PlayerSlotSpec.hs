module Parun.Codec.PlayerSlotSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.PlayerSlot as PlayerSlot
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerSlot as PlayerSlot
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayerSlot" $ do
  Spec.it s "MkPlayerSlot, each opponent (Riptide Gearhulk)" $
    Common.assertCodec
      s
      PlayerSlot.codec
      ( PlayerSlot.MkPlayerSlot
          { PlayerSlot.players = PlayerRelation.Opponent,
            PlayerSlot.slot = SlotName.MkSlotName (Text.pack "thatPlayer")
          }
      )
      " {\"players\":\"Opponent\",\"slot\":\"thatPlayer\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerSlot.codec
