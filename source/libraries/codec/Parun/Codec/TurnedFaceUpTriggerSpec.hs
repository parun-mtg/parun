module Parun.Codec.TurnedFaceUpTriggerSpec where

import qualified Parun.Codec.TurnedFaceUpTrigger as TurnedFaceUpTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.TurnedFaceUpTrigger as TurnedFaceUpTrigger

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TurnedFaceUpTrigger" $ do
  -- Skirk Marauder's "when this creature is turned face up".
  Spec.it s "writes no key for the defaults" $
    Common.assertCodec s TurnedFaceUpTrigger.codec TurnedFaceUpTrigger.defaultValue " {} "
  -- Growing Dread's "whenever you turn a permanent face up".
  Spec.it s "round-trips every field" $
    Common.assertCodec
      s
      TurnedFaceUpTrigger.codec
      (TurnedFaceUpTrigger.MkTurnedFaceUpTrigger (ActingPermanent.Matching (Filter.And [])) PlayerRelation.You)
      " {\"permanent\":{\"type\":\"Matching\",\"value\":{\"type\":\"And\",\"value\":[]}},\"player\":\"You\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TurnedFaceUpTrigger.codec
