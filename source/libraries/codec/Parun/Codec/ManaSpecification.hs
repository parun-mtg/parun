module Parun.Codec.ManaSpecification where

import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ManaSpecification as ManaSpecification

codec :: Codec.Codec ManaSpecification.ManaSpecification
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "AnyMana" ManaSpecification.AnyMana,
      Arm.nullary "ChosenColor" ManaSpecification.ChosenColor,
      Arm.payload "OfType" ManaType.codec ManaSpecification.OfType (\x -> case x of ManaSpecification.OfType y -> Just y; _ -> Nothing)
    ]

tagOf :: ManaSpecification.ManaSpecification -> String
tagOf x = case x of
  ManaSpecification.AnyMana {} -> "AnyMana"
  ManaSpecification.ChosenColor {} -> "ChosenColor"
  ManaSpecification.OfType {} -> "OfType"
