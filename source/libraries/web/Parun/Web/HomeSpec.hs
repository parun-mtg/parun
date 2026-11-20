module Parun.Web.HomeSpec where

import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Home" $ do
  let has piece home = Text.isInfixOf (Text.pack piece) (Html.toText (Home.view home))
      tapping label home = Html.labelled (Text.pack "button") (Text.pack label) page >>= \path -> Html.handler (Text.pack "click") path page
        where
          page = Home.view home
  Spec.describe s "view" $ do
    Spec.it s "offers Resume game only while a game is paused" $ do
      Spec.assertEqWith s "unpaused" (has "Resume game" Home.new) False
      Spec.assertEqWith s "paused" (has "Resume game" Home.new {Home.paused = True}) True

    Spec.it s "plays against bots and opens replays from their buttons" $ do
      Spec.assertEqWith s "bots" (tapping "Play against bots" Home.new) (Just (Home.MkChoose Home.MkPlayBots))
      Spec.assertEqWith s "replays" (tapping "Replays" Home.new) (Just (Home.MkChoose Home.MkReplays))

    Spec.it s "says how many cards loaded once it knows" $ do
      Spec.assertEqWith s "known" (has "14082 cards loaded." Home.new {Home.cards = Just 14082}) True
      Spec.assertEqWith s "unknown" (has "cards loaded" Home.new) False

    Spec.it s "shows a problem as an alert" $ do
      Spec.assertEq s (has "role=\"alert\">" Home.new {Home.problem = Just (Text.pack "Reload to try again.")} && has "Reload to try again." Home.new {Home.problem = Just (Text.pack "Reload to try again.")}) True

  Spec.describe s "update" $ do
    Spec.it s "goes where chosen while no game is paused" $ do
      Spec.assertEq s (Home.update (Home.MkChoose Home.MkPlayBots) Home.new) (Home.MkGo Home.MkPlayBots)

    Spec.it s "asks before conceding a paused game for a new one" $ do
      let paused = Home.new {Home.paused = True}
          asked = paused {Home.confirming = Just Home.MkPlayBots}
      Spec.assertEqWith s "asks" (Home.update (Home.MkChoose Home.MkPlayBots) paused) (Home.MkStay asked)
      Spec.assertEqWith s "the question shows" (has "Starting a new game concedes the one in progress." asked) True
      Spec.assertEqWith s "confirmed" (Home.update Home.MkConfirm asked) (Home.MkGo Home.MkPlayBots)
      Spec.assertEqWith s "cancelled" (Home.update Home.MkCancel asked) (Home.MkStay paused)

    Spec.it s "asks before conceding a paused game to play on from a replay" $ do
      let asked = Home.new {Home.paused = True, Home.confirming = Just (Home.MkPlayOn (Text.pack "a-scenario") 3)}
      Spec.assertEqWith s "the question shows" (has "Concede and play on" asked) True
      Spec.assertEqWith s "confirmed" (Home.update Home.MkConfirm asked) (Home.MkGo (Home.MkPlayOn (Text.pack "a-scenario") 3))

    Spec.it s "opens replays from a paused game without asking" $ do
      Spec.assertEq s (Home.update (Home.MkChoose Home.MkReplays) Home.new {Home.paused = True}) (Home.MkGo Home.MkReplays)

    Spec.it s "resumes the paused game" $ do
      Spec.assertEq s (Home.update Home.MkResume Home.new {Home.paused = True}) Home.MkBack
