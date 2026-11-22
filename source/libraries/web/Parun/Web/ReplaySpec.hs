module Parun.Web.ReplaySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Types.Answer as Answer
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Move as Move
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Reply as Reply
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.When as When
import qualified Parun.Web.History as History
import qualified Parun.Web.HistorySpec as HistorySpec
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
      Spec.assertEq s (tapped "button" "\x25B6", tapped "button" "\x25C0", tapped "button" "bob (Player 2)") (Just (Replay.MkGo 2), Just (Replay.MkGo 0), Just (Replay.MkSeat (PlayerId.MkPlayerId 1)))

    Spec.it s "names each seat to view as by its name and the name the table gives it" $ do
      let page = Html.toText (Replay.view frame)
      Spec.assertBool s (Text.isInfixOf (Text.pack ">alice (You)</button>") page) "the seat viewed as"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">bob (Player 2)</button>") page) "another"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">You</button>") (Html.toText (Replay.view frame {Replay.seats = [(PlayerId.MkPlayerId 0, Text.pack "You")]}))) "a name the table shares"

    Spec.it s "offers to play on from the position, and to go back to the list" $ do
      let page = Replay.view frame {Replay.position = 1}
          tapped label = Html.labelled (Text.pack "button") (Text.pack label) page >>= \path -> Html.handler (Text.pack "click") path page
      Spec.assertEqWith s "play on" (tapped "Play on from here") (Just Replay.MkBranch)
      Spec.assertEqWith s "back" (tapped "All replays") (Just Replay.MkBack)
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "&amp;branch") (Html.toText page))) "no branch link"

    Spec.it s "says an answer in a line, the entry as written behind Details" $ do
      let page = Html.toText . Replay.view $ frame {Replay.entry = Just (Text.pack "raw"), Replay.entryLine = Just (Text.pack "Turn 1 \x00b7 Untap \x00b7 You: shuffle")}
      Spec.assertBool s (Text.isInfixOf (Text.pack "<p class=\"entry-line\">Turn 1 \x00b7 Untap \x00b7 You: shuffle</p><details class=\"entry-details\"><summary>Details</summary><pre class=\"entry\">raw</pre></details>") page) "line, then details"
      Spec.assertBool s (Text.isInfixOf (Text.pack "<pre class=\"entry\">raw</pre>") (Html.toText (Replay.view frame {Replay.entry = Just (Text.pack "raw")}))) "raw alone without a line"

  Spec.describe s "lineOf" $ do
    Spec.it s "names the turn, the step, the seat and what it answers" $ do
      let at turn phase label = When.MkWhen turn phase (Label.MkLabel (Text.pack label)) 0
          answering prompt = Entry.Do (Move.Answer (Answer.MkAnswer (Text.pack prompt) (Reply.Array []) False))
          names = Map.fromList [(Text.pack "player1", Text.pack "You")]
      Spec.assertEqWith s "an answer" (Replay.lineOf names (Timed.MkTimed (at 1 (Phase.Beginning BeginningStep.Untap) "player1") Nothing (answering "Shuffle"))) (Just (Text.pack "Turn 1 \x00b7 Untap \x00b7 You: shuffle"))
      Spec.assertEqWith s "a concession, its seat unnamed" (Replay.lineOf names (Timed.MkTimed (at 3 Phase.PrecombatMain "player2") Nothing (Entry.Do Move.Concede))) (Just (Text.pack "Turn 3 \x00b7 Precombat main \x00b7 player2: concede"))
      Spec.assertEqWith s "a multiword prompt" (Replay.lineOf Map.empty (Timed.MkTimed (at 2 (Phase.Combat CombatStep.DeclareAttackers) "player1") Nothing (answering "DeclareAttackers"))) (Just (Text.pack "Turn 2 \x00b7 Declare attackers \x00b7 player1: declare attackers"))

  Spec.describe s "picker" $ do
    Spec.it s "lists the names holding what is typed, whatever its case" $ do
      let view = Replay.picker (Replay.listing []) {Replay.typed = Text.pack "PIKER"} (Just (Map.empty, fmap Text.pack ["goblin-piker-attacks", "shock-kills", "a-piker-blocks"]))
          page = Html.toText view
      Spec.assertEqWith s "match" (Html.labelled (Text.pack "button") (Text.pack "goblin-piker-attacks") view >>= \path -> Html.handler (Text.pack "click") path view) (Just (Replay.MkOpen (Text.pack "goblin-piker-attacks")))
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "href=\"?scenario=") page)) "no link"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "shock-kills") page)) "filtered out"
      Spec.assertBool s (Text.isInfixOf (Text.pack "2 of 3") page) "count"

    Spec.it s "lists the manifest's matching names under their themes ahead of every name" $ do
      let themes = Map.fromList [(Text.pack "Combat", fmap Text.pack ["a-piker-blocks", "shock-kills"]), (Text.pack "Burn", fmap Text.pack ["shock-kills"])]
          page = Html.toText $ Replay.picker (Replay.listing []) {Replay.typed = Text.pack "piker"} (Just (themes, fmap Text.pack ["goblin-piker-attacks", "shock-kills", "a-piker-blocks"]))
          at needle = Text.length (fst (Text.breakOn (Text.pack needle) page))
      Spec.assertBool s (at ">Combat<" < at "Every scenario" && at "Every scenario" < at "goblin-piker-attacks") "the theme first"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack ">Burn<") page)) "a theme with no match left out"
      Spec.assertBool s (at ">Combat<" < at ">a-piker-blocks<") "the theme's match"

    Spec.it s "says the scenarios are being fetched until they are" $ do
      Spec.assertBool s (Text.isInfixOf (Text.pack "Fetching the scenarios") (Html.toText (Replay.picker (Replay.listing []) Nothing))) "fetching"

  Spec.describe s "your games" $ do
    let games = [HistorySpec.game {History.ended = n} | n <- [1, 3, 2]]
        shown = Replay.listing games
        page listing = Html.toText (Replay.picker listing Nothing)
        tapping label listing = let view = Replay.picker listing Nothing in Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
    Spec.it s "shows the games finished here first, newest first" $ do
      Spec.assertEqWith s "the shelf" (Replay.shelf shown, Replay.shelf (Replay.listing [])) (Replay.MkYourGames, Replay.MkScenarios)
      Spec.assertEqWith s "newest first" (fmap History.ended (Replay.games shown)) [3, 2, 1]
      Spec.assertEqWith s "watch" (tapping "Watch" shown) (Just (Replay.MkWatch (Text.pack "3")))
      Spec.assertEqWith s "play again" (tapping "Play again" shown) (Just (Replay.MkPlayAgain (Text.pack "3")))
      Spec.assertEqWith s "to the scenarios" (tapping "Scenarios" shown) (Just (Replay.MkShow Replay.MkScenarios))

    Spec.it s "asks before deleting a game, and deletes only it once confirmed" $ do
      let asking = Replay.listed (Replay.MkForget (Replay.MkForgetGame (Text.pack "3"))) Text.empty shown
      Spec.assertEqWith s "delete" (tapping "Delete" shown) (Just (Replay.MkForget (Replay.MkForgetGame (Text.pack "3"))))
      Spec.assertBool s (Text.isInfixOf (Text.pack "Delete this game?") (page asking)) "asked"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Delete this game?") (page shown))) "not before"
      Spec.assertEqWith s "confirm" (tapping "Delete" asking) (Just Replay.MkConfirmForget)
      Spec.assertEqWith s "cancel" (Replay.listed Replay.MkCancelForget Text.empty asking) shown
      Spec.assertEqWith s "deleted" (fmap History.ended (Replay.games (Replay.forgot asking))) [2, 1]

    Spec.it s "asks before clearing every game" $ do
      let asking = Replay.listed (Replay.MkForget Replay.MkForgetAll) Text.empty shown
      Spec.assertEqWith s "clear" (tapping "Clear history" shown) (Just (Replay.MkForget Replay.MkForgetAll))
      Spec.assertBool s (Text.isInfixOf (Text.pack "Delete every game kept here?") (page asking)) "asked"
      Spec.assertEqWith s "confirm" (tapping "Delete all" asking) (Just Replay.MkConfirmForget)
      Spec.assertEqWith s "cleared" (Replay.games (Replay.forgot asking)) []

    Spec.it s "offers Play again only where this browser held every deck" $ do
      let invited = HistorySpec.game {History.seats = fmap (\seat -> seat {History.deckName = Nothing}) (History.seats HistorySpec.game)}
      Spec.assertEqWith s "invited" (tapping "Play again" (Replay.listing [invited])) Nothing

  Spec.describe s "ended" $ do
    Spec.it s "goes back to the position and plays on from it again" $ do
      let view = Replay.ended [Text.pack "Alice wins."]
          tapped label = Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
      Spec.assertEqWith s "back" (tapped "Back to the replay") (Just Replay.MkResumeReplay)
      Spec.assertEqWith s "again" (tapped "Play on again") (Just Replay.MkBranch)

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
      Replay.entryLine = Nothing,
      Replay.question = Nothing,
      Replay.failure = Nothing,
      Replay.reading = Nothing,
      Replay.written = Map.empty
    }
