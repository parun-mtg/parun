module Parun.Web.ReplaySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Replay as Replay

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Replay" $ do
  Spec.describe s "view" $ do
    Spec.it s "steps back only past the start and on only before the end" $ do
      let page at = Html.toText . Replay.view $ frame {Replay.position = at}
          off label at = Text.isInfixOf (Text.pack "disabled=\"\"") . Text.takeWhile (/= '>') . snd . Text.breakOn (Text.pack ("aria-label=\"" <> label <> "\"")) $ page at
      Spec.assertEq s (fmap (\label -> (off label 0, off label 3)) ["First position", "Previous position", "Next position", "Last position"]) [(True, False), (True, False), (False, True), (False, True)]

    Spec.it s "says where the run stands, and why it stopped short" $ do
      let page = Html.toText . Replay.view $ frame {Replay.position = 1, Replay.failure = Just (Text.pack "the entry was never reached")}
      Spec.assertBool s (Text.isInfixOf (Text.pack "Before entry 2 of 3") page) "position"
      Spec.assertBool s (Text.isInfixOf (Text.pack "role=\"alert\">the entry was never reached") page) "failure"

    Spec.it s "sends a step's position and a seat when tapped" $ do
      let page = Replay.view frame {Replay.position = 1}
          tapped name label = Html.labelled (Text.pack name) (Text.pack label) page >>= \path -> Html.handler (Text.pack "click") path page
      Spec.assertEq s (tapped "button" "\x25B6", tapped "button" "\x25C0", tapped "button" "bob") (Just (Replay.MkGo 2), Just (Replay.MkGo 0), Just (Replay.MkSeat (PlayerId.MkPlayerId 1)))

    Spec.it s "offers to play on from the position" $ do
      let page = Html.toText . Replay.view $ frame {Replay.position = 1}
      Spec.assertBool s (Text.isInfixOf (Text.pack "href=\"?scenario=a-scenario&amp;at=1&amp;branch\"") page) "the branch's link"

  Spec.describe s "picker" $ do
    Spec.it s "lists the names holding what is typed, whatever its case" $ do
      let page = Html.toText $ Replay.picker (Text.pack "PIKER") Map.empty (fmap Text.pack ["goblin-piker-attacks", "shock-kills", "a-piker-blocks"])
      Spec.assertBool s (Text.isInfixOf (Text.pack "href=\"?scenario=goblin-piker-attacks\"") page) "match"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "shock-kills") page)) "filtered out"
      Spec.assertBool s (Text.isInfixOf (Text.pack "2 of 3") page) "count"

    Spec.it s "lists the manifest's matching names under their themes ahead of every name" $ do
      let themes = Map.fromList [(Text.pack "Combat", fmap Text.pack ["a-piker-blocks", "shock-kills"]), (Text.pack "Burn", fmap Text.pack ["shock-kills"])]
          page = Html.toText $ Replay.picker (Text.pack "piker") themes (fmap Text.pack ["goblin-piker-attacks", "shock-kills", "a-piker-blocks"])
          at needle = Text.length (fst (Text.breakOn (Text.pack needle) page))
      Spec.assertBool s (at ">Combat<" < at "Every scenario" && at "Every scenario" < at "goblin-piker-attacks") "the theme first"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack ">Burn<") page)) "a theme with no match left out"
      Spec.assertBool s (at ">Combat<" < at "?scenario=a-piker-blocks") "the theme's match"

  Spec.describe s "ended" $ do
    Spec.it s "links back to the position and to play on from it again" $ do
      let page = Html.toText $ Replay.ended (Text.pack "a-scenario") 2 [Text.pack "Alice wins."]
      Spec.assertBool s (Text.isInfixOf (Text.pack "href=\"?scenario=a-scenario&amp;at=2\"") page) "back"
      Spec.assertBool s (Text.isInfixOf (Text.pack "href=\"?scenario=a-scenario&amp;at=2&amp;branch\"") page) "again"

frame :: Replay.Frame
frame =
  Replay.MkFrame
    { Replay.name = Text.pack "a-scenario",
      Replay.description = Text.pack "what it shows",
      Replay.seats = [(PlayerId.MkPlayerId 0, Text.pack "alice"), (PlayerId.MkPlayerId 1, Text.pack "bob")],
      Replay.viewer = PlayerId.MkPlayerId 0,
      Replay.position = 0,
      Replay.positions = 3,
      Replay.board = Nothing,
      Replay.depth = 0,
      Replay.entry = Nothing,
      Replay.question = Nothing,
      Replay.failure = Nothing,
      Replay.reading = Nothing
    }
