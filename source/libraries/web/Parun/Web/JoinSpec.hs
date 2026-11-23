module Parun.Web.JoinSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Join as Join
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Route as Route

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Join" $ do
  Spec.describe s "rematched" $ do
    Spec.it s "keeps the deck and the match-up, connected, with every player's ready flag cleared" $ do
      let played = (Join.new Route.MkGathering) {Join.picked = Just (Text.pack "saved/mine"), Join.problem = Just (Text.pack "old"), Join.places = [place "Player 1" (Just True) False, place "You" (Just True) False, place "Player 3" Nothing True]}
          back = Join.rematched played
      Spec.assertEq s (Join.picked back, Join.connected back, Join.problem back) (Just (Text.pack "saved/mine"), True, Nothing)
      Spec.assertEq s (fmap Link.ready (Join.places back)) [Just False, Just False, Nothing]
      Spec.assertBool s (not (Join.isReady back)) "not ready"
    Spec.it s "goes back to the lobby from a game rejoined, its deck chosen again" $ do
      Spec.assertEq s (Join.stage (Join.rematched (Join.new Route.MkUnderWay))) Route.MkGathering
  Spec.describe s "yourDeck" $ do
    Spec.it s "is the deck chosen, else the one the host says you play" $ do
      let held = (Join.new Route.MkGathering) {Join.places = [place "Player 1" (Just False) False, place "You" (Just False) False]}
          unchosen = (Join.new Route.MkGathering) {Join.places = [Link.MkPlace (Text.pack "You") Link.unchosen (Just False) False Set.empty]}
      Spec.assertEqWith s "the host's" (Join.yourDeck held) (Just (Text.pack "A deck"))
      Spec.assertEqWith s "none chosen anywhere" (Join.yourDeck unchosen) Nothing
      Spec.assertEqWith s "chosen here" (Join.yourDeck (Join.update (Join.MkPick mine) Text.empty decked)) (Just (Text.pack "Mine"))
  Spec.describe s "view" $ do
    Spec.it s "shows the decks as the lobby does: shelves, filters, tiles with their colours" $ do
      let shown = Html.toText (Join.view decked {Join.browser = (Join.browser decked) {DeckBrowser.shelf = DeckBrowser.MkSaved}})
      Spec.assertBool s (all ((`Text.isInfixOf` shown) . Text.pack) ["class=\"shelves\"", "class=\"colour-toggles\"", "class=\"deck-grid\"", "class=\"pips\""]) "the lobby's browser"
    Spec.it s "keeps Make my reply code off until a deck is chosen" $ do
      let replyButton screen = [b | b <- Html.elements (Text.pack "button") (Join.view screen), Html.content b == Text.pack "Make my reply code"]
          disabled b = Text.isInfixOf (Text.pack "disabled") (Html.toText b)
      Spec.assertEqWith s "no deck" (fmap disabled (replyButton decked)) [True]
      Spec.assertEqWith s "a deck" (fmap disabled (replyButton (Join.update (Join.MkPick mine) Text.empty decked))) [False]
    Spec.it s "asks no deck of a game under way, its reply code to make at once" $ do
      let underWay = decked {Join.stage = Route.MkUnderWay}
          shown = Html.toText (Join.view underWay)
      Spec.assertEqWith s "Make my reply code on" [Text.isInfixOf (Text.pack "disabled") (Html.toText b) | b <- Html.elements (Text.pack "button") (Join.view underWay), Html.content b == Text.pack "Make my reply code"] [False]
      Spec.assertBool s (not (any ((`Text.isInfixOf` shown) . Text.pack) ["Choose your deck", "class=\"deck-grid\"", "Your deck"])) "no deck picker"
      Spec.assertBool s (Join.canReply underWay) "the reply code made"
    Spec.it s "names your deck as the host holds it after a Rematch of a game rejoined" $ do
      let back = Join.rematched decked {Join.connected = True, Join.places = [place "Player 1" (Just True) False, place "You" (Just True) False]}
          shown = Html.toText (Join.view back)
      Spec.assertBool s (Text.isInfixOf (Text.pack "Your deck: A deck") shown && not (Text.isInfixOf (Text.pack "Choose your deck") shown)) "your deck named"
      Spec.assertEqWith s "Ready on" [Text.isInfixOf (Text.pack "disabled") (Html.toText b) | b <- Html.elements (Text.pack "button") (Join.view back), Html.content b == Text.pack "Ready"] [False]
    Spec.it s "names each seat as the game will, marking a bot's" $ do
      let shown = Html.toText (Join.view (Join.new Route.MkGathering) {Join.connected = True, Join.places = [place "Player 1" (Just True) False, place "You" (Just False) False, place "Player 3" Nothing True, place "Player 4" Nothing False]})
          slot who = Text.pack ("<span class=\"slot-title\">" <> who <> "</span>")
          bot = Text.pack "<span class=\"chip bot\">Bot</span>"
      Spec.assertEqWith s "the host" (Text.isInfixOf (slot "Player 1") shown) True
      Spec.assertEqWith s "one bot, after its name" (Text.count bot shown, Text.isInfixOf (slot "Player 3" <> bot) shown) (1, True)
    Spec.it s "gives the reply code a Copy button naming its box" $ do
      let shown = Html.toText (Join.view (Join.new Route.MkGathering) {Join.reply = Just (Text.pack "reply-code")})
      Spec.assertBool s (all ((`Text.isInfixOf` shown) . Text.pack) ["reply-code", "id=\"join-reply\"", "data-copy=\"join-reply\""]) "the code and its button"
  where
    mine = Text.pack "saved/mine"
    decked = (Join.new Route.MkGathering) {Join.browser = DeckBrowser.withDecks (Seq.fromList [mine, Text.pack "sets/fdc/calling-all-angels"]) Map.empty Map.empty DeckBrowser.new}
    place who ready bot = Link.MkPlace (Text.pack who) (Text.pack "A deck") ready bot Set.empty
