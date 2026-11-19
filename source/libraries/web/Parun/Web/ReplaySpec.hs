module Parun.Web.ReplaySpec where

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

  Spec.describe s "picker" $ do
    Spec.it s "lists the names holding what is typed, whatever its case" $ do
      let page = Html.toText $ Replay.picker (Text.pack "PIKER") (fmap Text.pack ["goblin-piker-attacks", "shock-kills", "a-piker-blocks"])
      Spec.assertBool s (Text.isInfixOf (Text.pack "href=\"?scenario=goblin-piker-attacks\"") page) "match"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "shock-kills") page)) "filtered out"
      Spec.assertBool s (Text.isInfixOf (Text.pack "2 of 3") page) "count"

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
      Replay.entry = Nothing,
      Replay.question = Nothing,
      Replay.failure = Nothing,
      Replay.reading = Nothing
    }
