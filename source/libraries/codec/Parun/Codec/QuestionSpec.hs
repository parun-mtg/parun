module Parun.Codec.QuestionSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Question as Question
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as Reply

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Question" $ do
  Spec.it s "every key" $ Common.assertCodec s Question.codec testQuestion testQuestionJson
  -- CR 724.1: a choice the seat makes for a player it controls.
  Spec.it s "for another player" $ Common.assertCodec s Question.codec testQuestion {Question.for = Just (PlayerId.MkPlayerId 0)} (init testQuestionJson <> ",\"for\":0}")
  -- CR 734.1: the reply taking back the announcement under way.
  Spec.it s "with a withdrawal" $ Common.assertCodec s Question.codec testQuestion {Question.withdraw = Just (Reply.Array [])} (init testQuestionJson <> ",\"withdraw\":[]}")
  -- CR 601.2h: a mana payment's, the null answer, is still one.
  Spec.it s "with a null withdrawal" $ Common.assertCodec s Question.codec testQuestion {Question.withdraw = Just Reply.Null} (init testQuestionJson <> ",\"withdraw\":null}")
  Spec.it s "has a schema" $ Common.assertHasSchema s Question.codec

testQuestion :: Question.Question
testQuestion =
  Question.MkQuestion
    { Question.seat = PlayerId.MkPlayerId 1,
      Question.kind = Text.pack "DeclareAttackers",
      Question.offer = Reply.Array [Reply.Text (Text.pack "$7")],
      Question.for = Nothing,
      Question.withdraw = Nothing
    }

testQuestionJson :: String
testQuestionJson = "{\"seat\":1,\"kind\":\"DeclareAttackers\",\"offer\":[\"$7\"]}"
