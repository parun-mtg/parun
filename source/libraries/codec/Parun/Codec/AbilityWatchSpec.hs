module Parun.Codec.AbilityWatchSpec where

import qualified Parun.Codec.AbilityWatch as AbilityWatch
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityWatch as AbilityWatch
import qualified Parun.Types.ChapterMoment as ChapterMoment
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.WatchedAbility as WatchedAbility

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AbilityWatch" $ do
  -- Tom Bombadil's payload.
  Spec.it s "MkAbilityWatch, both keys" $
    Common.assertCodec
      s
      AbilityWatch.codec
      ( AbilityWatch.MkAbilityWatch
          { AbilityWatch.ability = WatchedAbility.SagaFinalChapter ChapterMoment.Resolves,
            AbilityWatch.controller = PlayerRelation.You
          }
      )
      " {\"ability\":{\"type\":\"SagaFinalChapter\",\"value\":\"Resolves\"},\"controller\":\"You\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AbilityWatch.codec
