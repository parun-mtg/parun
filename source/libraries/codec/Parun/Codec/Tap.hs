{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Tap where

import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Tap as Tap

-- | The bound slot is ELIDED when absent, and the tapper when it is the
-- resolving controller.
codec :: Codec.Codec Tap.Tap
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec Tap.ref
  tapped <- Fields.defaulted "tapped" Nothing (Common.maybe SlotName.codec) Tap.tapped
  tapper <- Fields.defaulted "tapper" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec Tap.tapper
  pure Tap.MkTap {Tap.ref = ref, Tap.tapped = tapped, Tap.tapper = tapper}
