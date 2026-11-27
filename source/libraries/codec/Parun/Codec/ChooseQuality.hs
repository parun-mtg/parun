{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChooseQuality where

import qualified Data.Text as Text
import qualified Parun.Codec.ChosenQuality as ChosenQuality
import qualified Parun.Codec.QualityChooser as QualityChooser
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChooseQuality as ChooseQuality
import qualified Parun.Types.ChosenQuality as ChosenQuality.Type
import qualified Parun.Types.QualityChooser as QualityChooser.Type

-- | A bare object keyed by the record's field names, @linked@ and @secret@
-- defaulting to False and @chooser@ to the resolving controller. Refuses each
-- player choosing anything but a word, or in secret (ChooseQuality.chooser).
codec :: Codec.Codec ChooseQuality.ChooseQuality
codec = Fields.objectWith printed $ do
  quality <- Fields.required "quality" ChosenQuality.codec ChooseQuality.quality
  linked <- Fields.defaulted "linked" False Common.boolean ChooseQuality.linked
  secret <- Fields.defaulted "secret" False Common.boolean ChooseQuality.secret
  chooser <- Fields.defaulted "chooser" QualityChooser.Type.Controller QualityChooser.codec ChooseQuality.chooser
  pure
    ChooseQuality.MkChooseQuality
      { ChooseQuality.quality = quality,
        ChooseQuality.linked = linked,
        ChooseQuality.secret = secret,
        ChooseQuality.chooser = chooser
      }
  where
    printed choice = case (ChooseQuality.chooser choice, ChooseQuality.quality choice) of
      (QualityChooser.Type.Controller, _) -> Right choice
      (QualityChooser.Type.EachPlayer _, ChosenQuality.Type.Word _)
        | not (ChooseQuality.secret choice) -> Right choice
      (QualityChooser.Type.EachPlayer _, _) -> Left (Text.pack "ChooseQuality: each player chooses only a word, and not in secret")
