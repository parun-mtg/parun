{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PayAny where

import qualified Parun.Codec.PaidResource as PaidResource
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PayAny as PayAny

-- | A bare object keyed by the record's field names, @linked@ defaulting to
-- False.
codec :: Codec.Codec PayAny.PayAny
codec = Fields.object $ do
  resource <- Fields.required "resource" PaidResource.codec PayAny.resource
  slot <- Fields.required "slot" SlotName.codec PayAny.slot
  linked <- Fields.defaulted "linked" False Common.boolean PayAny.linked
  pure
    PayAny.MkPayAny
      { PayAny.resource = resource,
        PayAny.slot = slot,
        PayAny.linked = linked
      }
