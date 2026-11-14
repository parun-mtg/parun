{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AttachedToBound where

import qualified Data.Text as Text
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AttachedToBound as AttachedToBound
import qualified Parun.Types.Zone as Zone

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Parun.Codec.ObjectRef's AttachedToBound arm.
--
-- @zone@ is defaulted to the battlefield, and must be it or CR 400.7f's
-- graveyard.
codec :: Codec.Codec AttachedToBound.AttachedToBound
codec = Fields.objectWith validZone $ do
  slot <- Fields.required "slot" SlotName.codec AttachedToBound.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) AttachedToBound.filter
  zone <- Fields.defaulted "zone" Zone.Battlefield Zone.codec AttachedToBound.zone
  pure
    AttachedToBound.MkAttachedToBound
      { AttachedToBound.slot = slot,
        AttachedToBound.filter = filter_,
        AttachedToBound.zone = zone
      }

validZone :: AttachedToBound.AttachedToBound -> Either Text.Text AttachedToBound.AttachedToBound
validZone ref = case AttachedToBound.zone ref of
  Zone.Battlefield -> Right ref
  Zone.Graveyard -> Right ref
  zone -> Left (Text.pack ("AttachedToBound: nothing attached is found in " <> show zone))
