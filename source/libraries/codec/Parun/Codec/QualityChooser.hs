module Parun.Codec.QualityChooser where

import qualified Parun.Codec.ChoiceDistinct as ChoiceDistinct
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.QualityChooser as QualityChooser

codec :: Codec.Codec QualityChooser.QualityChooser
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Controller" QualityChooser.Controller,
      Arm.payload "EachPlayer" ChoiceDistinct.codec QualityChooser.EachPlayer (\x -> case x of QualityChooser.EachPlayer d -> Just d; _ -> Nothing)
    ]

tagOf :: QualityChooser.QualityChooser -> String
tagOf x = case x of
  QualityChooser.Controller -> "Controller"
  QualityChooser.EachPlayer _ -> "EachPlayer"
