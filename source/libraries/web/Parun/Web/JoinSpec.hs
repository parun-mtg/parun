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
      let played = Join.new {Join.picked = Just (Text.pack "saved/mine"), Join.problem = Just (Text.pack "old"), Join.places = [place "Player 1" (Just True) False, place "You" (Just True) False, place "Player 3" Nothing True]}
          back = Join.rematched played
      Spec.assertEq s (Join.picked back, Join.connected back, Join.problem back) (Just (Text.pack "saved/mine"), True, Nothing)
      Spec.assertEq s (fmap Link.ready (Join.places back)) [Just False, Just False, Nothing]
      Spec.assertBool s (not (Join.isReady back)) "not ready"
  Spec.describe s "view" $ do
    Spec.it s "names each seat as the game will, marking a bot's" $ do
      let shown = Html.toText (Join.view Join.new {Join.connected = True, Join.places = [place "Player 1" (Just True) False, place "You" (Just False) False, place "Player 3" Nothing True, place "Player 4" Nothing False]})
          slot who = Text.pack ("<span class=\"slot-title\">" <> who <> "</span>")
          bot = Text.pack "<span class=\"chip bot\">Bot</span>"
      Spec.assertEqWith s "the host" (Text.isInfixOf (slot "Player 1") shown) True
      Spec.assertEqWith s "one bot, after its name" (Text.count bot shown, Text.isInfixOf (slot "Player 3" <> bot) shown) (1, True)
    Spec.it s "gives the reply code a Copy button naming its box" $ do
      let shown = Html.toText (Join.view Join.new {Join.reply = Just (Text.pack "reply-code")})
      Spec.assertBool s (all ((`Text.isInfixOf` shown) . Text.pack) ["reply-code", "id=\"join-reply\"", "data-copy=\"join-reply\""]) "the code and its button"
  where
    place who = Link.MkPlace (Text.pack who) (Text.pack "A deck")
