{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChooseQuality where

import qualified Parun.Codec.ChosenQuality as ChosenQuality
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChooseQuality as ChooseQuality

-- | A bare object keyed by the record's field names, @linked@ defaulting to
-- False.
codec :: Codec.Codec ChooseQuality.ChooseQuality
codec = Fields.object $ do
  quality <- Fields.required "quality" ChosenQuality.codec ChooseQuality.quality
  linked <- Fields.defaulted "linked" False Common.boolean ChooseQuality.linked
  pure
    ChooseQuality.MkChooseQuality
      { ChooseQuality.quality = quality,
        ChooseQuality.linked = linked
      }
