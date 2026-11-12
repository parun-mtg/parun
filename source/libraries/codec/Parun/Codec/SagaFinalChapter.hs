{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SagaFinalChapter where

import qualified Parun.Codec.ChapterMoment as ChapterMoment
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.SagaFinalChapter as SagaFinalChapter

-- | A bare object keyed by the record's field names, both required: neither
-- half of the printed wording has a default.
codec :: Codec.Codec SagaFinalChapter.SagaFinalChapter
codec = Fields.object $ do
  moment <- Fields.required "moment" ChapterMoment.codec SagaFinalChapter.moment
  controller <- Fields.required "controller" PlayerRelation.codec SagaFinalChapter.controller
  pure SagaFinalChapter.MkSagaFinalChapter {SagaFinalChapter.moment = moment, SagaFinalChapter.controller = controller}
