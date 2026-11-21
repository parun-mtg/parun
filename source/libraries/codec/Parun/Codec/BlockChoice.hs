module Parun.Codec.BlockChoice where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.BlockChoice as BlockChoice

-- | CR 509.4's readings, as a deferred lot carries them
-- (Parun.Codec.DeferredLot).
codec :: Codec.Codec BlockChoice.BlockChoice
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "AnyAttacker" BlockChoice.AnyAttacker,
      Arm.payload "SpecifiedAttacker" ObjectId.codec BlockChoice.SpecifiedAttacker (\x -> case x of BlockChoice.SpecifiedAttacker y -> Just y; _ -> Nothing)
    ]

tagOf :: BlockChoice.BlockChoice -> String
tagOf x = case x of
  BlockChoice.AnyAttacker {} -> "AnyAttacker"
  BlockChoice.SpecifiedAttacker {} -> "SpecifiedAttacker"
