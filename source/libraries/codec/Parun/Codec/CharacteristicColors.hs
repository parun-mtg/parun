{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CharacteristicColors where

import qualified Data.Set as Set
import qualified Parun.Codec.Color as Color
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CharacteristicColors as CharacteristicColors

-- | A bare object keyed by the record's field names, each defaulted.
codec :: Codec.Codec CharacteristicColors.CharacteristicColors
codec = Fields.object $ do
  colors <- Fields.defaulted "colors" Set.empty (Common.set Color.codec) CharacteristicColors.colors
  chosen <- Fields.defaulted "chosen" False Common.boolean CharacteristicColors.chosen
  pure CharacteristicColors.MkCharacteristicColors {CharacteristicColors.colors = colors, CharacteristicColors.chosen = chosen}
