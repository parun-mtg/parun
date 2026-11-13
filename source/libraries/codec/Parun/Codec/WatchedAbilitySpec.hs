module Parun.Codec.WatchedAbilitySpec where

import qualified Parun.Codec.WatchedAbility as WatchedAbility
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChapterMoment as ChapterMoment
import qualified Parun.Types.WatchedAbility as WatchedAbility

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.WatchedAbility" $ do
  Spec.it s "SagaFinalChapter carries its moment" $
    Common.assertCodec
      s
      WatchedAbility.codec
      (WatchedAbility.SagaFinalChapter ChapterMoment.Triggers)
      " {\"type\":\"SagaFinalChapter\",\"value\":\"Triggers\"} "
  -- Firebender Ascension's payload.
  Spec.it s "CausedByOwnAttack is a bare tag" $
    Common.assertCodec
      s
      WatchedAbility.codec
      WatchedAbility.CausedByOwnAttack
      " {\"type\":\"CausedByOwnAttack\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s WatchedAbility.codec
