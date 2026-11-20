{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EachKind where

import qualified Parun.Codec.KindStep as KindStep
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.EachKind as EachKind

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Parun.Codec.AnotherCounter's EachKind arm.
codec :: Codec.Codec EachKind.EachKind
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec EachKind.slot
  step <- Fields.required "step" KindStep.codec EachKind.step
  pure EachKind.MkEachKind {EachKind.slot = slot, EachKind.step = step}
