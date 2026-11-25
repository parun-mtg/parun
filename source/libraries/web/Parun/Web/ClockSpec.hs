module Parun.Web.ClockSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Web.Clock as Clock
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Clock" $ do
  Spec.describe s "starting" $ do
    Spec.it s "gives each seat its minutes, and none untimed" $ do
      Spec.assertEqWith s "timed" (Clock.left (Clock.starting (Just 5) (Set.fromList [one, two]))) (Map.fromList [(one, 300), (two, 300)])
      Spec.assertEqWith s "untimed" (Clock.starting Nothing (Set.fromList [one, two])) Clock.none

  Spec.describe s "stop" $ do
    Spec.it s "charges only the seat whose clock ran, never below none" $ do
      let started = Clock.start one 100 (Clock.starting (Just 1) (Set.fromList [one, two]))
      Spec.assertEqWith s "charged" (Clock.left (Clock.stop 112.5 started)) (Map.fromList [(one, 47.5), (two, 60)])
      Spec.assertEqWith s "stopped" (Clock.running (Clock.stop 112.5 started)) Nothing
      Spec.assertEqWith s "never below none" (Map.lookup one (Clock.left (Clock.stop 500 started))) (Just 0)
      Spec.assertEqWith s "nothing running" (Clock.stop 500 (Clock.starting (Just 1) (Set.singleton one))) (Clock.starting (Just 1) (Set.singleton one))

  Spec.describe s "faces" $ do
    Spec.it s "reads the running clock down to now, saying when it runs out, and the others as they stand" $ do
      let started = Clock.start one 100 (Clock.starting (Just 1) (Set.fromList [one, two]))
      Spec.assertEqWith s "faces" (Clock.faces 110 1000000 started) [Clock.MkFace one 50 (Just 1050000), Clock.MkFace two 60 Nothing]

  Spec.describe s "shownTime" $ do
    Spec.it s "shows minutes and seconds, a part second counted whole" $ do
      Spec.assertEqWith s "times" (fmap Clock.shownTime [600, 599.2, 61, 9, 0.1, 0, -3]) ["10:00", "10:00", "1:01", "0:09", "0:01", "0:00", "0:00"]

  Spec.describe s "view" $ do
    Spec.it s "calls the viewer's clock You unless the screen names its seat, marking the running and the low" $ do
      let shown = Html.toText (Element.el "div" [] [] (Clock.view one False [Clock.MkFace one 30 (Just 5), Clock.MkFace two 600 Nothing]))
          named = Html.toText (Element.el "div" [] [] (Clock.view one True [Clock.MkFace one 30 Nothing]))
      Spec.assertBool s (Text.isInfixOf (Text.pack "class=\"clock running low\" role=\"timer\" title=\"Your clock\" data-ends=\"5\"") shown) ("yours, running and low: " <> Text.unpack shown)
      Spec.assertBool s (Text.isInfixOf (Text.pack ">You</span><span class=\"clock-time\">0:30<") shown) "You"
      Spec.assertBool s (Text.isInfixOf (Text.pack "class=\"clock\" role=\"timer\" title=\"Player 2's clock\"><span class=\"clock-who\">Player 2</span><span class=\"clock-time\">10:00<") shown) "the other's, stopped"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Player 1</span>") named) "named"

  Spec.describe s "ending" $ do
    Spec.it s "loses the game for the seat out of time and wins it for the other of two" $ do
      Spec.assertEqWith s "the seat out of time" (Clock.ending 2 two (Just two)) (Ending.MkLost, [Text.pack "Player 2 ran out of time."])
      Spec.assertEqWith s "the other" (fmap fst [Clock.ending 2 two (Just one), Clock.ending 2 two Nothing]) [Ending.MkWon, Ending.MkWonBy one]
    Spec.it s "stops a game of three for the rest (#9757)" $ do
      Spec.assertEqWith s "the rest" (Clock.ending 3 two (Just one)) (Ending.MkStopped, [Text.pack "The game stopped: Player 2 ran out of time."])
      Spec.assertEqWith s "the seat out of time" (fst (Clock.ending 3 two (Just two))) Ending.MkLost
  where
    one = PlayerId.MkPlayerId 0
    two = PlayerId.MkPlayerId 1
