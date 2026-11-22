module Parun.Web.HomeSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Home" $ do
  let has piece home = Text.isInfixOf (Text.pack piece) (Html.toText (Home.view home))
      -- A button whose text is the label, or a tile's whose title it is.
      tapping label home = Html.search titled page >>= \path -> Html.handler (Text.pack "click") path page
        where
          page = Home.view home
          titled node = Text.isPrefixOf (Text.pack "<button") (Html.toText node) && (Html.content node == Text.pack label || Text.isPrefixOf (Text.pack "<button class=\"tile") (Html.toText node) && Text.isPrefixOf (Text.pack label) (Html.content node))
      paused = Home.new {Home.paused = Just (Home.MkPaused (Just 11) [(Text.pack "You", 22), (Text.pack "Player 2", 14)])}
  Spec.describe s "view" $ do
    Spec.it s "offers Resume only while a game is paused, with its turn and life totals" $ do
      Spec.assertEqWith s "unpaused" (tapping "Resume" Home.new) Nothing
      Spec.assertEqWith s "paused" (tapping "Resume" paused) (Just Home.MkResume)
      Spec.assertEqWith s "turn" (has "<h1>Turn 11</h1>" paused) True
      Spec.assertEqWith s "lives" (has "<div class=\"seat-life\"><dt class=\"eyebrow\">Player 2</dt><dd>14</dd></div>" paused) True

    Spec.it s "leads from its tiles to a game, the builder and the replays" $ do
      Spec.assertEqWith s "bots" (tapping "Play a bot" Home.new) (Just (Home.MkChoose (Home.MkPlayBots Nothing)))
      Spec.assertEqWith s "friend" (tapping "Play a friend" Home.new) (Just (Home.MkChoose Home.MkPlayFriend))
      Spec.assertEqWith s "build" (tapping "Build a deck" Home.new) (Just (Home.MkChoose Home.MkBuildDeck))
      Spec.assertEqWith s "replays" (tapping "Replays" Home.new) (Just (Home.MkChoose Home.MkReplays))

    Spec.it s "leads Play a bot only while no game is paused" $ do
      Spec.assertEqWith s "fresh" (has "<button class=\"tile lead\"" Home.new) True
      Spec.assertEqWith s "paused" (has "<button class=\"tile lead\"" paused) False

    Spec.it s "lists the decks saved here, each to open on the Decks screen or edit" $ do
      let saved = Home.new {Home.saved = [Home.MkDeck (Text.pack "saved/red-deck-wins") (Set.singleton Color.Red) (Just 60)]}
      Spec.assertEqWith s "none" (has "Your decks" Home.new) False
      Spec.assertEqWith s "named" (has "Red Deck Wins" saved && has "60 cards" saved) True
      Spec.assertEqWith s "edit" (tapping "Edit" saved) (Just (Home.MkChoose (Home.MkEditDeck (Text.pack "saved/red-deck-wins"))))
      Spec.assertEqWith s "open" (tapping "Red Deck Wins" saved) (Just (Home.MkChoose (Home.MkBrowseDecks (Just (Text.pack "saved/red-deck-wins")))))

    Spec.it s "carries the Fan Content notice" $ do
      Spec.assertEq s (has "unofficial Fan Content" Home.new) True

    Spec.it s "shows a problem as an alert" $ do
      Spec.assertEq s (has "role=\"alert\">" Home.new {Home.problem = Just (Text.pack "Reload to try again.")} && has "Reload to try again." Home.new {Home.problem = Just (Text.pack "Reload to try again.")}) True

  Spec.describe s "navigate" $ do
    Spec.it s "takes the top bar's sections where the tiles go, and Home to itself" $ do
      Spec.assertEq s (fmap Home.navigate [minBound .. maxBound]) [Home.MkCancel, Home.MkChoose (Home.MkPlayBots Nothing), Home.MkChoose (Home.MkBrowseDecks Nothing), Home.MkChoose Home.MkReplays]

    Spec.it s "asks before Play concedes a paused game" $ do
      Spec.assertEq s (Home.update (Home.navigate Bar.MkPlay) paused) (Home.MkStay paused {Home.confirming = Just (Home.MkPlayBots Nothing)})

  Spec.describe s "update" $ do
    Spec.it s "goes where chosen while no game is paused" $ do
      Spec.assertEq s (Home.update (Home.MkChoose (Home.MkPlayBots Nothing)) Home.new) (Home.MkGo (Home.MkPlayBots Nothing))

    Spec.it s "asks before conceding a paused game for a new one" $ do
      let asked = paused {Home.confirming = Just Home.MkPlayFriend}
      Spec.assertEqWith s "asks" (Home.update (Home.MkChoose Home.MkPlayFriend) paused) (Home.MkStay asked)
      Spec.assertEqWith s "the question shows" (has "Starting a new game concedes the one in progress." asked) True
      Spec.assertEqWith s "confirmed" (Home.update Home.MkConfirm asked) (Home.MkGo Home.MkPlayFriend)
      Spec.assertEqWith s "cancelled" (Home.update Home.MkCancel asked) (Home.MkStay paused)

    Spec.it s "asks before conceding a paused game to play on from a replay" $ do
      let asked = paused {Home.confirming = Just (Home.MkPlayOn (Text.pack "a-scenario") 3)}
      Spec.assertEqWith s "the question shows" (has "Concede and play on" asked) True
      Spec.assertEqWith s "confirmed" (Home.update Home.MkConfirm asked) (Home.MkGo (Home.MkPlayOn (Text.pack "a-scenario") 3))

    Spec.it s "opens replays and the builder from a paused game without asking" $ do
      Spec.assertEqWith s "replays" (Home.update (Home.MkChoose Home.MkReplays) paused) (Home.MkGo Home.MkReplays)
      Spec.assertEqWith s "builder" (Home.update (Home.MkChoose Home.MkBuildDeck) paused) (Home.MkGo Home.MkBuildDeck)

    Spec.it s "resumes the paused game" $ do
      Spec.assertEq s (Home.update Home.MkResume paused) Home.MkBack
