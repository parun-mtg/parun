module Parun.Codec.SpendScope where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.SpendScope as SpendScope

codec :: Codec.Codec SpendScope.SpendScope
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "EveryPayment" SpendScope.EveryPayment,
      Arm.payload "Casting" (Filter.codec Keyword.codec) SpendScope.Casting (\x -> case x of SpendScope.Casting y -> Just y; _ -> Nothing),
      Arm.payload "Activating" (Filter.codec Keyword.codec) SpendScope.Activating (\x -> case x of SpendScope.Activating y -> Just y; _ -> Nothing)
    ]

tagOf :: SpendScope.SpendScope -> String
tagOf x = case x of
  SpendScope.EveryPayment {} -> "EveryPayment"
  SpendScope.Casting {} -> "Casting"
  SpendScope.Activating {} -> "Activating"
