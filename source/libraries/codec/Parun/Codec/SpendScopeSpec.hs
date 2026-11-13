module Parun.Codec.SpendScopeSpec where

import qualified Parun.Codec.SpendScope as SpendScope
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.SpendScope as SpendScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SpendScope" $ do
  Spec.it s "EveryPayment" $
    Common.assertCodec s SpendScope.codec SpendScope.EveryPayment " {\"type\":\"EveryPayment\"} "
  Spec.it s "Casting" $
    Common.assertCodec s SpendScope.codec (SpendScope.Casting (Filter.HasCardType CardType.Creature)) " {\"type\":\"Casting\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "Activating" $
    Common.assertCodec s SpendScope.codec (SpendScope.Activating (Filter.HasCardType CardType.Creature)) " {\"type\":\"Activating\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SpendScope.codec
