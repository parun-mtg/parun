module Parun.Codec.ManaScale where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.ManaScale as ManaScale

codec :: Codec.Codec ManaScale.ManaScale
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Keeps" ManaScale.Keeps,
      Arm.payload "Exactly" Common.natural ManaScale.Exactly (\x -> case x of ManaScale.Exactly y -> Just y; _ -> Nothing),
      Arm.payload "Times" Common.natural ManaScale.Times (\x -> case x of ManaScale.Times y -> Just y; _ -> Nothing)
    ]

tagOf :: ManaScale.ManaScale -> String
tagOf x = case x of
  ManaScale.Keeps {} -> "Keeps"
  ManaScale.Exactly {} -> "Exactly"
  ManaScale.Times {} -> "Times"
