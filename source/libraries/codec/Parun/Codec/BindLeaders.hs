{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.BindLeaders where

import qualified Parun.Codec.Measure as Measure
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.BindLeaders as BindLeaders

codec :: Codec.Codec BindLeaders.BindLeaders
codec = Fields.object $ do
  among <- Fields.required "among" SlotName.codec BindLeaders.among
  measure <- Fields.required "measure" Measure.codec BindLeaders.measure
  slot <- Fields.required "slot" SlotName.codec BindLeaders.slot
  pure
    BindLeaders.MkBindLeaders
      { BindLeaders.among = among,
        BindLeaders.measure = measure,
        BindLeaders.slot = slot
      }
