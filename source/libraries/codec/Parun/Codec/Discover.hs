{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Discover where

import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Discover as Discover
import qualified Parun.Types.PlayerRef as PlayerRef.Type
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | The player, ELIDED when it is you; the N; and the slot, ELIDED when
-- absent, as Parun.Codec.Surveil's is.
codec :: Codec.Codec Discover.Discover
codec = Fields.object $ do
  player <- Fields.defaulted "player" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec Discover.player
  quantity <- Fields.required "quantity" Quantity.codec Discover.quantity
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Discover.slot
  pure
    Discover.MkDiscover
      { Discover.player = player,
        Discover.quantity = quantity,
        Discover.slot = slot
      }
