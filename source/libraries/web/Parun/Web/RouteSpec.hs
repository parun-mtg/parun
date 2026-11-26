module Parun.Web.RouteSpec where

import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Replay as Web.Replay
import qualified Parun.Web.Route as Route

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Route" $ do
  let routes =
        [ Route.MkHome,
          Route.MkPlay,
          Route.MkJoin Route.MkGathering,
          Route.MkJoin Route.MkSeated,
          Route.MkJoin Route.MkUnderWay,
          Route.MkDecks Nothing,
          Route.MkDecks (Just (Text.pack "saved/My deck")),
          Route.MkBuild Nothing,
          Route.MkBuild (Just (Text.pack "starter/red")),
          Route.MkReplays,
          Route.MkReplay Route.MkWatching (Web.Replay.MkScenario (Text.pack "cards/lightning-bolt")) 3,
          Route.MkReplay Route.MkPlayingOn (Web.Replay.MkScenario (Text.pack "a&b=c")) 4,
          Route.MkReplay Route.MkWatching (Web.Replay.MkPlayed (Text.pack "1795364751000")) 12,
          Route.MkReplay Route.MkPlayingOn (Web.Replay.MkPlayed (Text.pack "1795364751000")) 1
        ]

  Spec.describe s "render" $ do
    Spec.it s "names the home screen by no query" $ do
      Spec.assertEq s (Route.render Route.MkHome) Text.empty

    Spec.it s "names a scenario's replay by its link, at its position" $ do
      Spec.assertEq s (Route.render (Route.MkReplay Route.MkWatching (Web.Replay.MkScenario (Text.pack "cards/lightning-bolt")) 3)) (Text.pack "?scenario=cards%2flightning-bolt&at=3")

    Spec.it s "names a game played on from a position by branch" $ do
      Spec.assertEq s (Route.render (Route.MkReplay Route.MkPlayingOn (Web.Replay.MkPlayed (Text.pack "17")) 2)) (Text.pack "?game=17&at=2&branch")

    Spec.it s "names the join screen by join, the offer left to the fragment" $ do
      Spec.assertEq s (Route.render (Route.MkJoin Route.MkGathering)) (Text.pack "?join")

    Spec.it s "names a seated game's join screen by seated" $ do
      Spec.assertEq s (Route.render (Route.MkJoin Route.MkSeated)) (Text.pack "?seated")

    Spec.it s "names a game under way's join screen by rejoin" $ do
      Spec.assertEq s (Route.render (Route.MkJoin Route.MkUnderWay)) (Text.pack "?rejoin")

  Spec.describe s "parse" $ do
    Spec.it s "reads back every route rendered" $ do
      Spec.assertEq s (fmap (Route.parse . Route.render) routes) routes

    Spec.it s "reads an older link's empty scenario as the replays" $ do
      Spec.assertEq s (Route.parse (Text.pack "?scenario=")) Route.MkReplays

    Spec.it s "reads a scenario with no position at its first" $ do
      Spec.assertEq s (Route.parse (Text.pack "?scenario=foo&level=Standard")) (Route.MkReplay Route.MkWatching (Web.Replay.MkScenario (Text.pack "foo")) 0)

    Spec.it s "reads a branch as played on, an older link's fillings passed over" $ do
      Spec.assertEq s (fmap (Route.parse . Text.pack) ["?game=17&branch", "?game=17&branch=here.bot-hard-aggro"]) (replicate 2 (Route.MkReplay Route.MkPlayingOn (Web.Replay.MkPlayed (Text.pack "17")) 0))

    Spec.it s "reads a query naming no screen as the home screen" $ do
      Spec.assertEq s (fmap (Route.parse . Text.pack) ["", "?level=Standard&offer=xyz", "?at=nope"]) [Route.MkHome, Route.MkHome, Route.MkHome]
