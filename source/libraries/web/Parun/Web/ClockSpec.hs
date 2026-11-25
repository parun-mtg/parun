module Parun.Web.ClockSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Web.Clock as Clock
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Clock" $ do
  Spec.describe s "starting" $ do
    Spec.it s "gives each seat its minutes, and none untimed" $ do
      Spec.assertEqWith s "timed" (Clock.left (Clock.starting (Just 5) (Set.fromList [one, two]))) (Map.fromList [(one, 300), (two, 300)])
      Spec.assertEqWith s "untimed" (Clock.starting Nothing (Set.fromList [one, two])) Clock.none

  Spec.describe s "resumed" $ do
    Spec.it s "reads each seat's time left as the last reply kept found it, or as the game started with none kept" $ do
      let fresh = Clock.starting (Just 5) (Set.fromList [one, two])
      Spec.assertEqWith s "kept" (Clock.resumed fresh [Map.fromList [(one, 290), (two, 300)], Map.fromList [(one, 290), (two, 281.5)]] Nothing) fresh {Clock.left = Map.fromList [(one, 290), (two, 281.5)]}
      Spec.assertEqWith s "none kept" (Clock.resumed fresh [] Nothing) fresh
    Spec.it s "charges the seat whose clock ran as the page was left the time it ran, from its time left as it started, never below none" $ do
      let fresh = Clock.starting (Just 5) (Set.fromList [one, two])
          kept = [Map.fromList [(one, 290), (two, 300 :: Double)]]
      Spec.assertEqWith s "charged" (Clock.left (Clock.resumed fresh kept (Just (Clock.MkKept two 280 1000000 (Just 1012500))))) (Map.fromList [(one, 290), (two, 267.5)])
      Spec.assertEqWith s "unstamped" (Clock.left (Clock.resumed fresh kept (Just (Clock.MkKept two 280 1000000 Nothing)))) (Map.fromList [(one, 290), (two, 280)])
      Spec.assertEqWith s "never below none" (Clock.left (Clock.resumed fresh kept (Just (Clock.MkKept two 280 1000000 (Just 9000000))))) (Map.fromList [(one, 290), (two, 0)])

  Spec.describe s "keeping" $ do
    Spec.it s "keeps the running clock's seat, its time left as it started, and its start in epoch time" $ do
      let started = Clock.start one 100 (Clock.starting (Just 1) (Set.fromList [one, two]))
      Spec.assertEqWith s "running" (Clock.keeping 112.5 1000000 started) (Just (Clock.MkKept one 60 987500 Nothing))
      Spec.assertEqWith s "stopped" (Clock.keeping 112.5 1000000 (Clock.stop 112.5 started)) Nothing

  Spec.describe s "stop" $ do
    Spec.it s "charges only the seat whose clock ran, never below none" $ do
      let started = Clock.start one 100 (Clock.starting (Just 1) (Set.fromList [one, two]))
      Spec.assertEqWith s "charged" (Clock.left (Clock.stop 112.5 started)) (Map.fromList [(one, 47.5), (two, 60)])
      Spec.assertEqWith s "stopped" (Clock.running (Clock.stop 112.5 started)) Nothing
      Spec.assertEqWith s "never below none" (Map.lookup one (Clock.left (Clock.stop 500 started))) (Just 0)
      Spec.assertEqWith s "nothing running" (Clock.stop 500 (Clock.starting (Just 1) (Set.singleton one))) (Clock.starting (Just 1) (Set.singleton one))

  Spec.describe s "banks" $ do
    Spec.it s "reads the running clock down to now, never below none, and the others as they stand" $ do
      let started = Clock.start one 100 (Clock.starting (Just 1) (Set.fromList [one, two]))
      Spec.assertEqWith s "banks" (Clock.banks 110 started) (Map.fromList [(one, 50), (two, 60)])
      Spec.assertEqWith s "never below none" (Clock.banks 500 started) (Map.fromList [(one, 0), (two, 60)])

  Spec.describe s "heard" $ do
    Spec.it s "keeps the banks sent, running the deciding seat's clock from now if it has one" $ do
      let sent = Map.fromList [(one, 50), (two, 60 :: Double)]
      Spec.assertEqWith s "timed" (Clock.heard 7 two sent) (Clock.MkClocks sent (Just (two, 7)) Nothing)
      Spec.assertEqWith s "untimed" (Clock.heard 7 (PlayerId.MkPlayerId 2) sent) (Clock.MkClocks sent Nothing Nothing)
      Spec.assertEqWith s "read on the guest's page" (Clock.faces 17 1000000 (Clock.heard 7 two sent)) [Clock.MkFace one 50 Nothing, Clock.MkFace two 50 (Just 1050000)]

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
  where
    one = PlayerId.MkPlayerId 0
    two = PlayerId.MkPlayerId 1
