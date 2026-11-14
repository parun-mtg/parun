module Parun.Codec.BindPlayersSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.BindPlayers as BindPlayers
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.BindPlayers as BindPlayers
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BindPlayers" $ do
  Spec.it s "round-trips" $
    Common.assertCodec
      s
      BindPlayers.codec
      (BindPlayers.MkBindPlayers {BindPlayers.players = PlayerRef.Relative PlayerRelation.AnyPlayer, BindPlayers.slot = SlotName.MkSlotName (Text.pack "contenders")})
      " {\"players\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"},\"slot\":\"contenders\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BindPlayers.codec
