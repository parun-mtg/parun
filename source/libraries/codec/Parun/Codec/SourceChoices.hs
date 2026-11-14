{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SourceChoices where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Parun.Codec.Acquisition as Acquisition
import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.CardType as CardType
import qualified Parun.Codec.Color as Color
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.SourceChoices as SourceChoices

codec :: Codec.Codec SourceChoices.SourceChoices
codec = Fields.object $ do
  names <- Fields.defaulted "names" Map.empty (Common.naturalMap PlayerId.codec (Common.set CardName.codec)) SourceChoices.names
  colors <- Fields.defaulted "colors" Set.empty (Common.set Color.codec) SourceChoices.colors
  subtype <- Fields.defaulted "subtype" Nothing (Common.maybe Subtype.codec) SourceChoices.subtype
  cardType <- Fields.defaulted "cardType" Nothing (Common.maybe CardType.codec) SourceChoices.cardType
  number <- Fields.defaulted "number" Nothing (Common.maybe Common.natural) SourceChoices.number
  acquisition <- Fields.defaulted "acquisition" Acquisition.Own Acquisition.codec SourceChoices.acquisition
  pure
    SourceChoices.MkSourceChoices
      { SourceChoices.names = names,
        SourceChoices.colors = colors,
        SourceChoices.subtype = subtype,
        SourceChoices.cardType = cardType,
        SourceChoices.number = number,
        SourceChoices.acquisition = acquisition
      }
