module Parun.Codec.SpendScopeSpec where

import qualified Parun.Codec.SpendScope as SpendScope
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActivationScope as ActivationScope
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
    Common.assertCodec s SpendScope.codec (SpendScope.Activating (ActivationScope.MkActivationScope (Filter.HasCardType CardType.Creature) Nothing)) " {\"type\":\"Activating\",\"value\":{\"source\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}}} "
  Spec.it s "Activating lent abilities" $
    Common.assertCodec s SpendScope.codec (SpendScope.Activating (ActivationScope.MkActivationScope Filter.IsSource (Just (Filter.HasCardType CardType.Creature)))) " {\"type\":\"Activating\",\"value\":{\"lentBy\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"source\":{\"type\":\"IsSource\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SpendScope.codec
