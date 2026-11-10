module Parun.Codec.TargetedSubjectSpec where

import qualified Parun.Codec.TargetedSubject as TargetedSubject
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.TargetedSubject as TargetedSubject

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TargetedSubject" $ do
  Spec.it s "Permanent" $
    Common.assertCodec s TargetedSubject.codec (TargetedSubject.Permanent ActingPermanent.Self) " {\"type\":\"Permanent\",\"value\":{\"type\":\"Self\"}} "
  Spec.it s "Player" $
    Common.assertCodec s TargetedSubject.codec (TargetedSubject.Player PlayerRelation.You) " {\"type\":\"Player\",\"value\":\"You\"} "
  Spec.it s "PlayerOrPermanent" $
    Common.assertCodec s TargetedSubject.codec (TargetedSubject.PlayerOrPermanent PlayerRelation.AnyPlayer (ActingPermanent.Matching Filter.IsToken)) " {\"type\":\"PlayerOrPermanent\",\"value\":{\"player\":\"AnyPlayer\",\"permanent\":{\"type\":\"Matching\",\"value\":{\"type\":\"IsToken\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TargetedSubject.codec
