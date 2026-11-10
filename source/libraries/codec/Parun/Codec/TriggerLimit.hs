module Parun.Codec.TriggerLimit where

import qualified Data.Maybe as Maybe
import qualified Parun.Codec.SpentOn as SpentOn
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.SpentOn as SpentOn.Type
import qualified Parun.Types.TriggerLimit as TriggerLimit

-- | OncePerTurn's spender is ELIDED when it is the printed "This ability
-- triggers only once each turn" (SpentOn.Triggering), so only CR 603.2h's
-- "Do this only once each turn" writes a value.
codec :: Codec.Codec TriggerLimit.TriggerLimit
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Unlimited" TriggerLimit.Unlimited,
      Arm.optionalPayload
        "OncePerTurn"
        SpentOn.codec
        (TriggerLimit.OncePerTurn . Maybe.fromMaybe SpentOn.Type.Triggering)
        ( \x -> case x of
            TriggerLimit.OncePerTurn spender -> Just (if spender == SpentOn.Type.Triggering then Nothing else Just spender)
            _ -> Nothing
        ),
      Arm.nullary "OncePerGame" TriggerLimit.OncePerGame
    ]

tagOf :: TriggerLimit.TriggerLimit -> String
tagOf x = case x of
  TriggerLimit.Unlimited -> "Unlimited"
  TriggerLimit.OncePerTurn {} -> "OncePerTurn"
  TriggerLimit.OncePerGame -> "OncePerGame"
