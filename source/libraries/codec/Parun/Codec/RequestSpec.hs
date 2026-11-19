module Parun.Codec.RequestSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.PlayerViewSpec as PlayerViewSpec
import qualified Parun.Codec.QuestionSpec as QuestionSpec
import qualified Parun.Codec.Request as Request
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Request as Request

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Request" $ do
  Spec.it s "every key" $
    Common.assertCodec
      s
      Request.codec
      Request.MkRequest
        { Request.view = Just PlayerViewSpec.testView,
          Request.question = QuestionSpec.testQuestion,
          Request.problem = Just (Text.pack "expected an array"),
          Request.depth = 0
        }
      ("{\"view\":" <> PlayerViewSpec.testViewJson <> ",\"question\":" <> QuestionSpec.testQuestionJson <> ",\"problem\":\"expected an array\"}")
  Spec.it s "no view before the game" $
    Common.assertCodec
      s
      Request.codec
      Request.MkRequest {Request.view = Nothing, Request.question = QuestionSpec.testQuestion, Request.problem = Nothing, Request.depth = 0}
      ("{\"view\":null,\"question\":" <> QuestionSpec.testQuestionJson <> ",\"problem\":null}")
  -- CR 730.1a: a question a subgame raises.
  Spec.it s "a subgame's question" $
    Common.assertCodec
      s
      Request.codec
      Request.MkRequest {Request.view = Nothing, Request.question = QuestionSpec.testQuestion, Request.problem = Nothing, Request.depth = 1}
      ("{\"view\":null,\"question\":" <> QuestionSpec.testQuestionJson <> ",\"problem\":null,\"depth\":1}")
  Spec.it s "has a schema" $ Common.assertHasSchema s Request.codec
