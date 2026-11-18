module Parun.Codec.RecipientRef where

import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.RecipientRef as RecipientRef

-- | Reached from card data through Parun.Codec.RequireAttack's `defender` key
-- and Parun.Codec.ChangeTargets' `onto`.
codec :: Codec.Codec RecipientRef.RecipientRef
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Players" PlayerRef.codec RecipientRef.Players (\x -> case x of RecipientRef.Players y -> Just y; _ -> Nothing),
      Arm.payload "Permanents" ObjectRef.codec RecipientRef.Permanents (\x -> case x of RecipientRef.Permanents y -> Just y; _ -> Nothing)
    ]

tagOf :: RecipientRef.RecipientRef -> String
tagOf x = case x of
  RecipientRef.Players {} -> "Players"
  RecipientRef.Permanents {} -> "Permanents"
