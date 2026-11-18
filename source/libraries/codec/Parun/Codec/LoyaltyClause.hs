module Parun.Codec.LoyaltyClause where

import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.LoyaltyClause as LoyaltyClause

-- | Tagged, since TimesEachTurn carries a count.
codec :: Codec.Codec LoyaltyClause.LoyaltyClause
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Window" LoyaltyClause.Window,
      Arm.nullary "OncePerTurn" LoyaltyClause.OncePerTurn,
      Arm.payload "TimesEachTurn" Common.natural LoyaltyClause.TimesEachTurn (\x -> case x of LoyaltyClause.TimesEachTurn y -> Just y; _ -> Nothing)
    ]

tagOf :: LoyaltyClause.LoyaltyClause -> String
tagOf x = case x of
  LoyaltyClause.Window {} -> "Window"
  LoyaltyClause.OncePerTurn {} -> "OncePerTurn"
  LoyaltyClause.TimesEachTurn {} -> "TimesEachTurn"
