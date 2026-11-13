{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ControlPlayerNext where

import qualified Parun.Codec.ControlSpan as ControlSpan
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ControlPlayerNext as ControlPlayerNext

-- | An object keyed by the record's field names.
codec :: Codec.Codec ControlPlayerNext.ControlPlayerNext
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec ControlPlayerNext.slot
  during <- Fields.required "during" ControlSpan.codec ControlPlayerNext.during
  pure
    ControlPlayerNext.MkControlPlayerNext
      { ControlPlayerNext.slot = slot,
        ControlPlayerNext.during = during
      }
