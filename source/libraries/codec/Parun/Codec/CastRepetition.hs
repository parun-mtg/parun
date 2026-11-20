module Parun.Codec.CastRepetition where

import qualified Parun.Codec.TotalCap as TotalCap
import qualified Parun.Codec.UpToCasts as UpToCasts
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CastRepetition as CastRepetition

-- | Tagged rather than an enum, since two arms carry a payload.
codec :: Codec.Codec CastRepetition.CastRepetition
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Once" CastRepetition.Once,
      Arm.nullary "AnyNumber" CastRepetition.AnyNumber,
      Arm.payload "WithinTotal" TotalCap.codec CastRepetition.WithinTotal (\x -> case x of CastRepetition.WithinTotal y -> Just y; _ -> Nothing),
      Arm.payload "UpTo" UpToCasts.codec CastRepetition.UpTo (\x -> case x of CastRepetition.UpTo y -> Just y; _ -> Nothing)
    ]

tagOf :: CastRepetition.CastRepetition -> String
tagOf x = case x of
  CastRepetition.Once {} -> "Once"
  CastRepetition.AnyNumber {} -> "AnyNumber"
  CastRepetition.WithinTotal {} -> "WithinTotal"
  CastRepetition.UpTo {} -> "UpTo"
