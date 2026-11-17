module Parun.Codec.ChosenQuality where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.CardType as CardType
import qualified Parun.Codec.ChoiceDistinct as ChoiceDistinct
import qualified Parun.Codec.ChoiceOrdinal as ChoiceOrdinal
import qualified Parun.Codec.ChooseBetween as ChooseBetween
import qualified Parun.Codec.ExiledPile as ExiledPile
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.Codec.SubtypeFamily as SubtypeFamily
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChoiceDistinct as ChoiceDistinct
import qualified Parun.Types.ChoiceOrdinal as ChoiceOrdinal
import qualified Parun.Types.ChosenQuality as ChosenQuality

codec :: Codec.Codec ChosenQuality.ChosenQuality
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Colors" ChooseBetween.codec ChosenQuality.Colors (\x -> case x of ChosenQuality.Colors y -> Just y; _ -> Nothing),
      Arm.payload "Subtype" subtype (\(ordinal, distinct, family, among) -> ChosenQuality.Subtype ordinal distinct family among) (\x -> case x of ChosenQuality.Subtype ordinal distinct family among -> Just (ordinal, distinct, family, among); _ -> Nothing),
      Arm.optionalPayload "CardType" cardType (uncurry ChosenQuality.CardType . Maybe.fromMaybe ([], Nothing)) (\x -> case x of ChosenQuality.CardType [] Nothing -> Just Nothing; ChosenQuality.CardType except among -> Just (Just (except, among)); _ -> Nothing),
      Arm.payload "Number" number (uncurry ChosenQuality.Number) (\x -> case x of ChosenQuality.Number least most -> Just (least, most); _ -> Nothing),
      Arm.nullary "Parity" ChosenQuality.Parity,
      Arm.payload "Word" (Common.nonEmpty Common.text) ChosenQuality.Word (\x -> case x of ChosenQuality.Word y -> Just y; _ -> Nothing),
      Arm.nullary "Letter" ChosenQuality.Letter,
      Arm.payload "ExiledCard" exiledCard (uncurry ChosenQuality.ExiledCard) (\x -> case x of ChosenQuality.ExiledCard pile cards -> Just (pile, cards); _ -> Nothing)
    ]
  where
    subtype =
      Fields.object $
        (,,,)
          <$> Fields.defaulted "ordinal" ChoiceOrdinal.First ChoiceOrdinal.codec (\(o, _, _, _) -> o)
          <*> Fields.defaulted "distinct" ChoiceDistinct.MayRepeat ChoiceDistinct.codec (\(_, d, _, _) -> d)
          <*> Fields.required "family" SubtypeFamily.codec (\(_, _, f, _) -> f)
          <*> Fields.defaulted "among" Nothing (Common.maybe (Common.nonEmpty Subtype.codec)) (\(_, _, _, a) -> a)
    -- No value offers every card type (Umori, the Collector).
    cardType =
      Fields.object $
        (,)
          <$> Fields.defaulted "except" [] (Common.list CardType.codec) fst
          <*> Fields.defaulted "among" Nothing (Common.maybe (Common.nonEmpty CardType.codec)) snd
    -- An absent "most" is no maximum ("a number greater than 0").
    number =
      Fields.objectWith (\(least, most) -> if maybe False (least >) most then Left (Text.pack "Number: least must not exceed most") else Right (least, most)) $
        (,)
          <$> Fields.required "least" Common.natural fst
          <*> Fields.defaulted "most" Nothing (Common.maybe Common.natural) snd
    exiledCard =
      Fields.object $
        (,)
          <$> Fields.required "pile" ExiledPile.codec fst
          <*> Fields.required "filter" (Filter.codec Keyword.codec) snd

tagOf :: ChosenQuality.ChosenQuality -> String
tagOf x = case x of
  ChosenQuality.Colors {} -> "Colors"
  ChosenQuality.Subtype {} -> "Subtype"
  ChosenQuality.Number {} -> "Number"
  ChosenQuality.CardType {} -> "CardType"
  ChosenQuality.Parity -> "Parity"
  ChosenQuality.Word {} -> "Word"
  ChosenQuality.Letter -> "Letter"
  ChosenQuality.ExiledCard {} -> "ExiledCard"
