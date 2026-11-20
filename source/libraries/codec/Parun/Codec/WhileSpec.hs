module Parun.Codec.WhileSpec where

import qualified Parun.Codec.While as While
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.While as While

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.While" $ do
  -- CR 611.2b's baked seat and the condition it is checked as. Runtime-only:
  -- the one thing that serialises this is a DelayedTrigger (CR 603.7b).
  Spec.it s "MkWhile, both keys" $
    Common.assertCodec
      s
      While.codec
      ( While.MkWhile
          { While.player = PlayerId.MkPlayerId 0,
            While.condition = Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)),
            While.unbegun = Nothing
          }
      )
      " {\"player\":0,\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":0},\"comparison\":\"Exactly\",\"threshold\":{\"type\":\"Literal\",\"value\":0}}}} "
  -- CR 611.2b: a duration armed during a resolution that has yet to begin it.
  Spec.it s "MkWhile, unbegun" $
    Common.assertCodec
      s
      While.codec
      ( While.MkWhile
          { While.player = PlayerId.MkPlayerId 0,
            While.condition = Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)),
            While.unbegun = Just (ObjectId.MkObjectId 7)
          }
      )
      " {\"player\":0,\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":0},\"comparison\":\"Exactly\",\"threshold\":{\"type\":\"Literal\",\"value\":0}}},\"unbegun\":7} "
  Spec.it s "has a schema" $ Common.assertHasSchema s While.codec
