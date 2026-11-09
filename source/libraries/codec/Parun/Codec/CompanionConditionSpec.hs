module Parun.Codec.CompanionConditionSpec where

import qualified Parun.Codec.CompanionCondition as CompanionCondition
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CompanionCondition as CompanionCondition
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (CompanionCondition.CompanionCondition Keyword.Keyword)
codec = CompanionCondition.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CompanionCondition" $ do
  Spec.it s "EachCard" $
    Common.assertCodec
      s
      codec
      (CompanionCondition.EachCard (Filter.HasActivatedAbility Nothing))
      " {\"type\":\"EachCard\",\"value\":{\"type\":\"HasActivatedAbility\"}} "
  Spec.it s "CardsOverMinimum" $
    Common.assertCodec
      s
      codec
      (CompanionCondition.CardsOverMinimum 20)
      " {\"type\":\"CardsOverMinimum\",\"value\":20} "
  Spec.it s "DifferentNames" $
    Common.assertCodec
      s
      codec
      (CompanionCondition.DifferentNames (Filter.Not (Filter.HasCardType CardType.Land)))
      " {\"type\":\"DifferentNames\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Land\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
