module Parun.Codec.ForbiddenAct where

import qualified Parun.Codec.LimitedAct as LimitedAct
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ForbiddenAct as ForbiddenAct

codec :: Codec.Codec ForbiddenAct.ForbiddenAct
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "GainLife" ForbiddenAct.GainLife,
      Arm.nullary "LoseLife" ForbiddenAct.LoseLife,
      Arm.payload "MoreThanEachTurn" limit (uncurry ForbiddenAct.MoreThanEachTurn) (\x -> case x of ForbiddenAct.MoreThanEachTurn y z -> Just (y, z); _ -> Nothing),
      Arm.nullary "PayLifeToCastOrActivate" ForbiddenAct.PayLifeToCastOrActivate,
      Arm.nullary "BecomeMonarch" ForbiddenAct.BecomeMonarch
    ]
  where
    limit =
      Fields.object $
        (,)
          <$> Fields.required "act" LimitedAct.codec fst
          <*> Fields.required "count" Common.natural snd

tagOf :: ForbiddenAct.ForbiddenAct -> String
tagOf x = case x of
  ForbiddenAct.GainLife -> "GainLife"
  ForbiddenAct.LoseLife -> "LoseLife"
  ForbiddenAct.MoreThanEachTurn {} -> "MoreThanEachTurn"
  ForbiddenAct.PayLifeToCastOrActivate -> "PayLifeToCastOrActivate"
  ForbiddenAct.BecomeMonarch -> "BecomeMonarch"
