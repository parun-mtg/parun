module Parun.Web.JoinSpec where

import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Join as Join
import qualified Parun.Web.Link as Link

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Join" $ do
  Spec.describe s "rematched" $ do
    Spec.it s "keeps the deck and the match-up, connected, with every player's ready flag cleared" $ do
      let played = Join.new {Join.picked = Just (Text.pack "saved/mine"), Join.problem = Just (Text.pack "old"), Join.places = [place "Host" (Just True), place "You" (Just True), place "Bot 3" Nothing]}
          back = Join.rematched played
      Spec.assertEq s (Join.picked back, Join.connected back, Join.problem back) (Just (Text.pack "saved/mine"), True, Nothing)
      Spec.assertEq s (fmap Link.ready (Join.places back)) [Just False, Just False, Nothing]
      Spec.assertBool s (not (Join.isReady back)) "not ready"
  Spec.describe s "view" $ do
    Spec.it s "gives the reply code a Copy button naming its box" $ do
      let shown = Html.toText (Join.view Join.new {Join.reply = Just (Text.pack "reply-code")})
      Spec.assertBool s (all ((`Text.isInfixOf` shown) . Text.pack) ["reply-code", "id=\"join-reply\"", "data-copy=\"join-reply\""]) "the code and its button"
  where
    place who = Link.MkPlace (Text.pack who) (Text.pack "A deck")
