module Parun.Codec.ManaInstead where

import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ManaInstead as ManaInstead

codec :: Codec.Codec ManaInstead.ManaInstead
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "OfType" ManaType.codec ManaInstead.OfType (\x -> case x of ManaInstead.OfType y -> Just y; _ -> Nothing),
      Arm.nullary "AnyColor" ManaInstead.AnyColor
    ]

tagOf :: ManaInstead.ManaInstead -> String
tagOf x = case x of
  ManaInstead.OfType {} -> "OfType"
  ManaInstead.AnyColor {} -> "AnyColor"
