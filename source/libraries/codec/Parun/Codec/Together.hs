module Parun.Codec.Together where

import qualified Parun.Codec.DifferentIn as DifferentIn
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.Together as Together

codec :: Codec.Codec Together.Together
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "InOneZone" Together.InOneZone,
      Arm.payload "Differing" DifferentIn.codec Together.Differing (\x -> case x of Together.Differing y -> Just y; _ -> Nothing)
    ]

-- | Total, which is what makes a new constructor an error here rather than a
-- silent @{}@ -- 'Arm.tagged' says why it takes this beside the arm list.
tagOf :: Together.Together -> String
tagOf x = case x of
  Together.InOneZone -> "InOneZone"
  Together.Differing _ -> "Differing"
