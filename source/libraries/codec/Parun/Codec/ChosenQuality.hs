module Parun.Codec.ChosenQuality where

import qualified Parun.Codec.ChooseBetween as ChooseBetween
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.Codec.SubtypeFamily as SubtypeFamily
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChosenQuality as ChosenQuality

codec :: Codec.Codec ChosenQuality.ChosenQuality
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Colors" ChooseBetween.codec ChosenQuality.Colors (\x -> case x of ChosenQuality.Colors y -> Just y; _ -> Nothing),
      Arm.payload "Subtype" subtype (uncurry ChosenQuality.Subtype) (\x -> case x of ChosenQuality.Subtype family among -> Just (family, among); _ -> Nothing),
      Arm.nullary "CardType" ChosenQuality.CardType,
      Arm.payload "Number" ChooseBetween.codec ChosenQuality.Number (\x -> case x of ChosenQuality.Number y -> Just y; _ -> Nothing),
      Arm.nullary "Parity" ChosenQuality.Parity,
      Arm.payload "Word" (Common.nonEmpty Common.text) ChosenQuality.Word (\x -> case x of ChosenQuality.Word y -> Just y; _ -> Nothing)
    ]
  where
    subtype =
      Fields.object $
        (,)
          <$> Fields.required "family" SubtypeFamily.codec fst
          <*> Fields.defaulted "among" Nothing (Common.maybe (Common.nonEmpty Subtype.codec)) snd

tagOf :: ChosenQuality.ChosenQuality -> String
tagOf x = case x of
  ChosenQuality.Colors {} -> "Colors"
  ChosenQuality.Subtype {} -> "Subtype"
  ChosenQuality.Number {} -> "Number"
  ChosenQuality.CardType -> "CardType"
  ChosenQuality.Parity -> "Parity"
  ChosenQuality.Word {} -> "Word"
