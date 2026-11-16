{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Cloak where

import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Cloak as Cloak

-- | The library is optional: omitted, each instructed player cloaks from their
-- own. The slot is elided when absent, ManifestDread's.
codec :: Codec.Codec Cloak.Cloak
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec Cloak.player
  library <- Fields.defaulted "library" Nothing (Common.maybe PlayerRef.codec) Cloak.library
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Cloak.slot
  pure
    Cloak.MkCloak
      { Cloak.player = player,
        Cloak.library = library,
        Cloak.slot = slot
      }
