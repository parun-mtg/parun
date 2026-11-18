module Parun.Codec.GrantCopyCastSpec where

import qualified Parun.Codec.GrantCopyCast as GrantCopyCast
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.GrantCopyCast as GrantCopyCast
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.GrantCopyCast" $ do
  -- Every default elided: CR 109.5's "you", the printed colours, no recopy.
  Spec.it s "defaults elided" $
    Common.assertCodec
      s
      GrantCopyCast.codec
      (GrantCopyCast.MkGrantCopyCast Duration.UntilEndOfTurn (PlayerRef.Relative PlayerRelation.You) ManaSpending.AsProduced Nothing)
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"}} "
  -- Every field written.
  Spec.it s "every field written" $
    Common.assertCodec
      s
      GrantCopyCast.codec
      (GrantCopyCast.MkGrantCopyCast Duration.Indefinite (PlayerRef.Relative PlayerRelation.AnyPlayer) ManaSpending.AnyType (Just (Condition.All [])))
      " {\"duration\":{\"type\":\"Indefinite\"},\"player\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"},\"spending\":\"AnyType\",\"recopy\":{\"type\":\"All\",\"value\":[]}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s GrantCopyCast.codec
