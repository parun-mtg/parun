{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ManifestDread where

import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ManifestDread as ManifestDread

-- | A bare object keyed by the record's field names, the slot ELIDED when
-- absent as Blight's is.
codec :: Codec.Codec ManifestDread.ManifestDread
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec ManifestDread.player
  quantity <- Fields.required "quantity" Quantity.codec ManifestDread.quantity
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) ManifestDread.slot
  pure
    ManifestDread.MkManifestDread
      { ManifestDread.player = player,
        ManifestDread.quantity = quantity,
        ManifestDread.slot = slot
      }
