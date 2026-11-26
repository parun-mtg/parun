module Parun.Web.HomeSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.PlayerViewSpec as PlayerViewSpec
import qualified Parun.Codec.SeatViewSpec as SeatViewSpec
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.History as History
import qualified Parun.Web.HistorySpec as HistorySpec
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Replay as Replay

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Home" $ do
  let has piece home = Text.isInfixOf (Text.pack piece) (Html.toText (Home.view home))
      -- A button whose text is the label, or a tile's whose title it is.
      tapping label home = Html.search titled page >>= \path -> Html.handler (Text.pack "click") path page
        where
          page = Home.view home
          titled node = Text.isPrefixOf (Text.pack "<button") (Html.toText node) && (Html.content node == Text.pack label || Text.isPrefixOf (Text.pack "<button class=\"tile") (Html.toText node) && Text.isPrefixOf (Text.pack label) (Html.content node))
      paused = Home.new {Home.paused = Just (Home.MkPaused (Just 11) [(Text.pack "You", Just (Text.pack "Elves (Decks)"), 22), (Text.pack "Player 2", Nothing, 14)])}
  Spec.describe s "view" $ do
    Spec.it s "offers Resume only while a game is paused, with its turn and life totals" $ do
      Spec.assertEqWith s "unpaused" (tapping "Resume" Home.new) Nothing
      Spec.assertEqWith s "paused" (tapping "Resume" paused) (Just Home.MkResume)
      Spec.assertEqWith s "turn" (has "<h1>Turn 11</h1>" paused) True
      Spec.assertEqWith s "lives" (has "<div class=\"seat-life\"><dt class=\"eyebrow\">Player 2</dt><dd>14</dd></div>" paused) True
      Spec.assertEqWith s "a deck" (has "<div class=\"seat-life\"><dt class=\"eyebrow\">You</dt><dd>22</dd><dd class=\"seat-deck muted\">Elves (Decks)</dd></div>" paused) True

  Spec.describe s "pausedOn" $ do
    Spec.it s "names each seat as the game does, with its deck" $ do
      let board = PlayerViewSpec.testView {PlayerView.seats = [SeatViewSpec.testSeat, SeatViewSpec.testSeat {SeatView.player = PlayerId.MkPlayerId 1, SeatView.life = 14}]}
          decks = [Text.pack "Elves (Decks)", Text.pack "Goblins (Decks)"]
      Spec.assertEqWith s "by seat" (Home.pausedOn decks (Just board)) (Home.MkPaused (Just 3) [(Text.pack "You", Just (Text.pack "Elves (Decks)"), 20), (Text.pack "Player 2", Just (Text.pack "Goblins (Decks)"), 14)])
      Spec.assertEqWith s "a guest's" (Home.pausedOn decks (Just board {PlayerView.viewer = PlayerId.MkPlayerId 1})) (Home.MkPaused (Just 3) [(Text.pack "Player 1", Just (Text.pack "Elves (Decks)"), 20), (Text.pack "You", Just (Text.pack "Goblins (Decks)"), 14)])
      Spec.assertEqWith s "decks unknown" (Home.pausedOn [] (Just board)) (Home.MkPaused (Just 3) [(Text.pack "You", Nothing, 20), (Text.pack "Player 2", Nothing, 14)])

    Spec.it s "leads from its tiles to a game, the builder and the replays" $ do
      Spec.assertEqWith s "a game" (tapping "New game" Home.new) (Just (Home.MkChoose (Home.MkNewGame Nothing)))
      Spec.assertEqWith s "no choice of opponent up front" (fmap (`has` Home.new) ["Play a bot", "Play a friend"]) [False, False]
      Spec.assertEqWith s "build" (tapping "Build a deck" Home.new) (Just (Home.MkChoose Home.MkBuildDeck))
      Spec.assertEqWith s "replays" (tapping "Replays" Home.new) (Just (Home.MkChoose Home.MkReplays))

    Spec.it s "leads New game only while no game is paused" $ do
      Spec.assertEqWith s "fresh" (has "<button class=\"tile lead\"" Home.new) True
      Spec.assertEqWith s "paused" (has "<button class=\"tile lead\"" paused) False

    Spec.it s "lists the decks saved here, each to open on the Decks screen or edit" $ do
      let saved = Home.new {Home.saved = [Home.MkDeck (Text.pack "saved/red-deck-wins") (Text.pack "Red Deck Wins") (Set.singleton Color.Red) (Just 60)]}
      Spec.assertEqWith s "none" (has "Your decks" Home.new) False
      Spec.assertEqWith s "named" (has "Red Deck Wins" saved && has "60 cards" saved) True
      Spec.assertEqWith s "edit" (tapping "Edit" saved) (Just (Home.MkChoose (Home.MkEditDeck (Text.pack "saved/red-deck-wins"))))
      Spec.assertEqWith s "open" (tapping "Red Deck Wins" saved) (Just (Home.MkChoose (Home.MkBrowseDecks (Just (Text.pack "saved/red-deck-wins")))))

    Spec.it s "lists the last games finished here, each to watch, and the way to all of them" $ do
      let games = [HistorySpec.game {History.ended = n} | n <- [10, 9 .. 1]]
          recent = Home.new {Home.finished = games}
          rows = length (Text.breakOnAll (Text.pack "class=\"game-row\"") (Html.toText (Home.view recent)))
      Spec.assertEqWith s "none" (has "Recent games" Home.new) False
      Spec.assertEqWith s "the last five" (has "Recent games" recent, rows) (True, Home.recentCount)
      Spec.assertEqWith s "watch" (tapping "Watch" recent) (Just (Home.MkChoose (Home.MkWatch (Replay.MkPlayed (Text.pack "10")))))
      Spec.assertEqWith s "all" (tapping "All games" recent) (Just (Home.MkChoose Home.MkReplays))

    Spec.it s "carries the Fan Content notice" $ do
      Spec.assertEq s (has "unofficial Fan Content" Home.new) True

    Spec.it s "shows a problem as an alert" $ do
      Spec.assertEq s (has "role=\"alert\">" Home.new {Home.problem = Just (Text.pack "Reload to try again.")} && has "Reload to try again." Home.new {Home.problem = Just (Text.pack "Reload to try again.")}) True

  Spec.describe s "navigate" $ do
    Spec.it s "takes the top bar's sections where the tiles go, and Home to itself" $ do
      Spec.assertEq s (fmap Home.navigate [minBound .. maxBound]) [Home.MkCancel, Home.MkChoose (Home.MkNewGame Nothing), Home.MkChoose (Home.MkBrowseDecks Nothing), Home.MkChoose Home.MkReplays]

    Spec.it s "opens the lobby from a paused game, which asks at Start" $ do
      Spec.assertEq s (Home.update (Home.navigate Bar.MkPlay) paused) (Home.MkGo (Home.MkNewGame Nothing))

  Spec.describe s "update" $ do
    Spec.it s "goes where chosen while no game is paused" $ do
      Spec.assertEq s (Home.update (Home.MkChoose (Home.MkNewGame Nothing)) Home.new) (Home.MkGo (Home.MkNewGame Nothing))

    Spec.it s "opens the lobby from a paused game without asking, which asks at Start" $ do
      Spec.assertEqWith s "new game" (Home.update (Home.MkChoose (Home.MkNewGame Nothing)) paused) (Home.MkGo (Home.MkNewGame Nothing))
      Spec.assertEqWith s "play again" (Home.update (Home.MkChoose (Home.MkPlayAgain (pure (Text.pack "a-deck")))) paused) (Home.MkGo (Home.MkPlayAgain (pure (Text.pack "a-deck"))))

    Spec.it s "opens replays and the builder from a paused game without asking" $ do
      Spec.assertEqWith s "replays" (Home.update (Home.MkChoose Home.MkReplays) paused) (Home.MkGo Home.MkReplays)
      Spec.assertEqWith s "builder" (Home.update (Home.MkChoose Home.MkBuildDeck) paused) (Home.MkGo Home.MkBuildDeck)

    Spec.it s "resumes the paused game" $ do
      Spec.assertEq s (Home.update Home.MkResume paused) Home.MkBack
