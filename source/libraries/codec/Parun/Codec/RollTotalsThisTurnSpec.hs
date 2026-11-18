module Parun.Codec.RollTotalsThisTurnSpec where

import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.RollTotalsThisTurn as RollTotalsThisTurn
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.RollTotalsThisTurn as RollTotalsThisTurn

-- | Instantiated at 'Quantity.Quantity', as Parun.Codec.Quantity does.
codec :: Codec.Codec (RollTotalsThisTurn.RollTotalsThisTurn Quantity.Quantity)
codec = RollTotalsThisTurn.codec Quantity.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RollTotalsThisTurn" $ do
  Spec.it s "MkRollTotalsThisTurn" $
    Common.assertCodec
      s
      codec
      ( RollTotalsThisTurn.MkRollTotalsThisTurn
          { RollTotalsThisTurn.roller = PlayerRef.Relative PlayerRelation.You,
            RollTotalsThisTurn.total = Quantity.Literal 7
          }
      )
      " {\"roller\":{\"type\":\"Relative\",\"value\":\"You\"},\"total\":{\"type\":\"Literal\",\"value\":7}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
