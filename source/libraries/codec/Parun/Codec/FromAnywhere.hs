{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.FromAnywhere where

import qualified Parun.Codec.Reach as Reach
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.FromAnywhere as FromAnywhere
import qualified Parun.Types.Reach as Reach

-- | A bare object keyed by the record's field names, @among@ omitted when it
-- names no slot and @reach@ when it is CR 400.7j's own 'Reach.PublicZone'.
codec :: Codec.Codec FromAnywhere.FromAnywhere
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec FromAnywhere.slot
  among <- Fields.defaulted "among" Nothing (Common.maybe SlotName.codec) FromAnywhere.among
  reach <- Fields.defaulted "reach" Reach.PublicZone Reach.codec FromAnywhere.reach
  pure
    FromAnywhere.MkFromAnywhere
      { FromAnywhere.slot = slot,
        FromAnywhere.among = among,
        FromAnywhere.reach = reach
      }
