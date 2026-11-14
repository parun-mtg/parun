module Parun.Codec.ManaProduction where

import qualified Data.Maybe as Maybe
import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.ColorSpread as ColorSpread
import qualified Parun.Codec.CouldProduce as CouldProduce
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.ManaCount as ManaCount
import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.ColorSpread as ColorSpread
import qualified Parun.Types.ManaProduction as ManaProduction

-- | @AnyColor@'s spread is optional, its absence CR 105.4's one colour, which
-- is what nearly every printing means.
codec :: Codec.Codec ManaProduction.ManaProduction
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "OfType" ManaType.codec ManaProduction.OfType (\x -> case x of ManaProduction.OfType y -> Just y; _ -> Nothing),
      Arm.payload "OneOf" (Common.set ManaType.codec) ManaProduction.OneOf (\x -> case x of ManaProduction.OneOf y -> Just y; _ -> Nothing),
      Arm.optionalPayload "AnyColor" ColorSpread.codec (ManaProduction.AnyColor . Maybe.fromMaybe ColorSpread.OneColor) (\x -> case x of ManaProduction.AnyColor ColorSpread.OneColor -> Just Nothing; ManaProduction.AnyColor y -> Just (Just y); _ -> Nothing),
      Arm.nullary "Chosen" ManaProduction.Chosen,
      Arm.nullary "SnowSymbol" ManaProduction.SnowSymbol,
      Arm.nullary "Noted" ManaProduction.Noted,
      Arm.payload "CouldProduce" CouldProduce.codec ManaProduction.CouldProduce (\x -> case x of ManaProduction.CouldProduce y -> Just y; _ -> Nothing),
      Arm.payload "Pooled" ManaCount.codec ManaProduction.Pooled (\x -> case x of ManaProduction.Pooled y -> Just y; _ -> Nothing),
      Arm.nullary "ProducedByTapped" ManaProduction.ProducedByTapped,
      Arm.nullary "InCommanderIdentity" ManaProduction.InCommanderIdentity,
      Arm.nullary "Circled" ManaProduction.Circled,
      Arm.payload "DraftNoted" CardName.codec ManaProduction.DraftNoted (\x -> case x of ManaProduction.DraftNoted y -> Just y; _ -> Nothing),
      Arm.payload "EachColorAmong" (Filter.codec Keyword.codec) ManaProduction.EachColorAmong (\x -> case x of ManaProduction.EachColorAmong y -> Just y; _ -> Nothing)
    ]

tagOf :: ManaProduction.ManaProduction -> String
tagOf x = case x of
  ManaProduction.OfType {} -> "OfType"
  ManaProduction.OneOf {} -> "OneOf"
  ManaProduction.AnyColor {} -> "AnyColor"
  ManaProduction.Chosen {} -> "Chosen"
  ManaProduction.SnowSymbol {} -> "SnowSymbol"
  ManaProduction.Noted {} -> "Noted"
  ManaProduction.CouldProduce {} -> "CouldProduce"
  ManaProduction.Pooled {} -> "Pooled"
  ManaProduction.ProducedByTapped {} -> "ProducedByTapped"
  ManaProduction.InCommanderIdentity {} -> "InCommanderIdentity"
  ManaProduction.Circled {} -> "Circled"
  ManaProduction.DraftNoted {} -> "DraftNoted"
  ManaProduction.EachColorAmong {} -> "EachColorAmong"
