module Parun.Codec.SagaFinalChapterSpec where

import qualified Parun.Codec.SagaFinalChapter as SagaFinalChapter
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChapterMoment as ChapterMoment
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SagaFinalChapter as SagaFinalChapter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.SagaFinalChapter" $ do
  -- Tom Bombadil's payload.
  Spec.it s "MkSagaFinalChapter, both keys" $
    Common.assertCodec
      s
      SagaFinalChapter.codec
      ( SagaFinalChapter.MkSagaFinalChapter
          { SagaFinalChapter.moment = ChapterMoment.Resolves,
            SagaFinalChapter.controller = PlayerRelation.You
          }
      )
      " {\"moment\":\"Resolves\",\"controller\":\"You\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SagaFinalChapter.codec
