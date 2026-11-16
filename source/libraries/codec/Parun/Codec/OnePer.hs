module Parun.Codec.OnePer where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.OnePer as OnePer

codec :: Codec.Codec OnePer.OnePer
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "ColorAmong" (Filter.codec Keyword.codec) OnePer.ColorAmong (\x -> case x of OnePer.ColorAmong y -> Just y; _ -> Nothing),
      Arm.nullary "CardType" OnePer.CardType
    ]

tagOf :: OnePer.OnePer -> String
tagOf x = case x of
  OnePer.ColorAmong {} -> "ColorAmong"
  OnePer.CardType {} -> "CardType"
