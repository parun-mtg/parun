module Parun.Web.ReplaySpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Spec as Spec
import qualified Parun.Types.Answer as Answer
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Move as Move
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as Reply
import qualified Parun.Types.Request as Request
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
      Spec.assertEq s (fmap (\label -> (off label 0, off label 3)) ["First position", "Previous turn", "Previous position", "Next position", "Next turn", "Last position"]) [(True, False), (True, False), (True, False), (False, True), (False, True), (False, True)]

    Spec.it s "steps a turn at a time, to the turn's first position or the end" $ do
      let tapped label at = let page = Replay.view frame {Replay.position = at} in step label page >>= \path -> Html.handler (Text.pack "click") path page
          going label at =
            tapped label at >>= \event -> case event of
              Replay.MkStep way -> Just (Replay.stepTo (Replay.turns frame) (Replay.positions frame) at way)
              _ -> Nothing
      Spec.assertEqWith s "next turn" (fmap (going "Next turn") [0, 1, 2]) (fmap Just [2, 2, 3])
      Spec.assertEqWith s "previous turn" (fmap (going "Previous turn") [1, 2, 3]) (fmap Just [0, 0, 2])

    Spec.it s "steps from where it stands when read, so taps queued meanwhile add up" $ do
      let from :: Natural -> [Replay.Step] -> Natural
          from = Foldable.foldl' (Replay.stepTo [0, 2, 4] 6)
      Spec.assertEqWith s "three steps on" (from 1 (replicate 3 Replay.MkStepOn)) 4
      Spec.assertEqWith s "two turns on" (from 1 [Replay.MkTurnOn, Replay.MkTurnOn]) 4
      Spec.assertEqWith s "two turns back" (from 5 [Replay.MkTurnBack, Replay.MkTurnBack]) 2
      Spec.assertEqWith s "held at the ends" (from 5 [Replay.MkStepOn, Replay.MkStepOn], from 1 [Replay.MkStepBack, Replay.MkStepBack]) (6, 0)
      Spec.assertEqWith s "the ends" (from 3 [Replay.MkToEnd], from 3 [Replay.MkToStart]) (6, 0)

    Spec.it s "tells its steps apart: an icon over a word, and a tooltip saying where each goes" $ do
      let page at = Replay.view frame {Replay.position = at, Replay.turns = [0, 2, 4], Replay.positions = 6}
          shown label at = Maybe.fromMaybe (Text.empty, Nothing) $ do
            path <- step label (page at)
            node <- Html.at path (page at)
            case node of
              Html.MkElement n -> pure (Html.content node, Html.propertyOf (Text.pack "title") n)
              Html.MkText _ -> Nothing
          caption label = fst (shown label 3)
          tooltip label at = Maybe.fromMaybe Text.empty (snd (shown label at))
      Spec.assertEqWith s "captions" (fmap caption ["First position", "Previous turn", "Previous position", "Next position", "Next turn", "Last position"]) (fmap Text.pack ["Start", "Turn", "Step", "Step", "Turn", "End"])
      Spec.assertEqWith s "the start" (tooltip "First position" 3) (Text.pack "To the start of the replay (<)")
      Spec.assertEqWith s "this turn" (tooltip "Previous turn" 3) (Text.pack "To the start of this turn ([)")
      Spec.assertEqWith s "the turn before" (tooltip "Previous turn" 4) (Text.pack "To the start of the turn before ([)")
      Spec.assertEqWith s "the first turn" (tooltip "Previous turn" 1) (Text.pack "To the start of the first turn ([)")
      Spec.assertEqWith s "no later turn" (tooltip "Next turn" 5) (Text.pack "To the end: no turn begins after this one (])")
      Spec.assertBool s (not (Text.any (`elem` "\x23EE\x23EA\x25C0\x25B6\x23E9\x23ED") (Html.toText (page 3)))) "no media glyphs, which some browsers draw as emoji"

    Spec.it s "says where the run stands, and why it stopped short" $ do
      let page = Html.toText . Replay.view $ frame {Replay.position = 1, Replay.failure = Just (Text.pack "the entry was never reached")}
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Step 1 of 3<") page) "position"
      Spec.assertBool s (Text.isInfixOf (Text.pack "role=\"alert\">the entry was never reached") page) "failure"

    Spec.it s "sends a step's way and a seat when tapped" $ do
      let page = Replay.view frame {Replay.position = 1}
          tapped name label = Html.labelled (Text.pack name) (Text.pack label) page >>= \path -> Html.handler (Text.pack "click") path page
          stepped label = step label page >>= \path -> Html.handler (Text.pack "click") path page
      Spec.assertEq s (stepped "Next position", stepped "Previous position", tapped "button" "Player 2") (Just (Replay.MkStep Replay.MkStepOn), Just (Replay.MkStep Replay.MkStepBack), Just (Replay.MkSeat (PlayerId.MkPlayerId 1)))

    Spec.it s "plays itself until the end, marking the page for page.js to step it" $ do
      let page playing at = Replay.view frame {Replay.playing = playing, Replay.position = at}
          tapped label playing at = step label (page playing at) >>= \path -> Html.handler (Text.pack "click") path (page playing at)
          marked playing at = Text.isInfixOf (Text.pack "<main class=\"game replay playing\"") (Html.toText (page playing at))
          off label at = Text.isInfixOf (Text.pack "disabled=\"\"") . Text.takeWhile (/= '>') . snd . Text.breakOn (Text.pack ("aria-label=\"" <> label <> "\"")) . Html.toText $ page False at
      Spec.assertEqWith s "play" (tapped "Play" False 1) (Just (Replay.MkPlay True))
      Spec.assertEqWith s "pause" (tapped "Pause" True 1) (Just (Replay.MkPlay False))
      Spec.assertEqWith s "marked while playing, before the end" (fmap (uncurry marked) [(True, 1), (False, 1), (True, 3)]) [True, False, False]
      Spec.assertEqWith s "nothing to play at the end" (off "Play" 1, off "Play" 3) (False, True)

    Spec.it s "names each seat to view as by its name and the name the table gives it" $ do
      let page = Html.toText (Replay.view frame)
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Player 1 (You)</button>") page) "the seat viewed as"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Player 2</button>") page) "another"
      Spec.assertBool s (Text.isInfixOf (Text.pack ">You</button>") (Html.toText (Replay.view frame {Replay.seats = [(PlayerId.MkPlayerId 0, Text.pack "You")]}))) "a name the table shares"

    Spec.it s "names a scenario's seats as the table does, never by their labels" $ do
      Spec.assertEqWith s "to view as" (Replay.named Map.empty labelled) [(PlayerId.MkPlayerId 0, Text.pack "Player 1"), (PlayerId.MkPlayerId 1, Text.pack "Player 2")]
      Spec.assertEqWith s "a game's names" (Replay.named (Map.singleton (Text.pack "alice") (Text.pack "You")) labelled) [(PlayerId.MkPlayerId 0, Text.pack "You"), (PlayerId.MkPlayerId 1, Text.pack "Player 2")]
      let line viewed = Replay.lineOf (Replay.tableNames (PlayerId.MkPlayerId viewed) labelled) True Nothing (Timed.MkTimed (When.MkWhen 3 Phase.PrecombatMain (Label.MkLabel (Text.pack "alice")) 0) Nothing (Entry.Do Move.Pass))
      Spec.assertEqWith s "in a line, to the seat viewed as" (line 0, line 1) (Text.pack "Turn 3 \x00b7 Precombat main \x00b7 You pass", Text.pack "Turn 3 \x00b7 Precombat main \x00b7 Player 1 passes")

    Spec.it s "offers to play on from the position, and to go back to the list" $ do
      let page = Replay.view frame {Replay.position = 1}
          tapped label = Html.labelled (Text.pack "button") (Text.pack label) page >>= \path -> Html.handler (Text.pack "click") path page
      Spec.assertEqWith s "play on" (tapped "Play on from here") (Just Replay.MkBranch)
      Spec.assertEqWith s "back" (tapped "All replays") (Just Replay.MkBack)
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "&amp;branch") (Html.toText page))) "no branch link"

    Spec.it s "says the entry in a line, and nothing as the engine writes it" $ do
      let page = Html.toText . Replay.view $ frame {Replay.entryLine = Just (Text.pack "Turn 1 \x00b7 Untap \x00b7 a library is shuffled")}
      Spec.assertBool s (Text.isInfixOf (Text.pack "<p class=\"entry-line\">Turn 1 \x00b7 Untap \x00b7 a library is shuffled</p>") page) "line"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "<details") page)) "no details"

    Spec.it s "says how the game ended only at the end" $ do
      let page at = Html.toText (Replay.view frame {Replay.position = at, Replay.outcome = Text.pack "You win."})
      Spec.assertEqWith s "shown" (fmap (Text.isInfixOf (Text.pack ">You win.<") . page) [0, 2, 3]) [False, False, True]

    Spec.it s "names the seat asked elsewhere without the question's kind" $ do
      let page = Html.toText (Replay.view frame {Replay.question = Just (request 1 "ChooseAction" (Reply.Array []))})
      Spec.assertBool s (Text.isInfixOf (Text.pack ">Waiting on Player 2.</p>") page) "waiting"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "ChooseAction") page)) "no kind"

    Spec.it s "draws the question asked of the seat viewed as, and nothing in it answers" $ do
      let shown = Replay.view frame {Replay.question = Just (request 0 "DeclareMulligan" (Reply.Object []))}
          page = Html.toText shown
          recorded = fst (Text.breakOn (Text.pack "</section>") (snd (Text.breakOn (Text.pack "<section class=\"recorded\"") page)))
      Spec.assertBool s (Text.isInfixOf (Text.pack "Keep this hand, or mulligan?") recorded) "the question"
      Spec.assertBool s (Text.isInfixOf (Text.pack "disabled=\"\">Keep</button>") recorded) "keep, disabled"
      Spec.assertEqWith s "keep does nothing" (Html.labelled (Text.pack "button") (Text.pack "Keep") shown >>= \path -> Html.handler (Text.pack "click") path shown) Nothing
      Spec.assertEqWith s "every button disabled" (Text.count (Text.pack "<button") recorded) (Text.count (Text.pack "disabled=\"\">") recorded)

  Spec.describe s "titled" $ do
    Spec.it s "titles a scenario by its description less a leading rules citation" $ do
      Spec.assertEqWith s "cited" (Replay.titled (Text.pack "x") (Text.pack "CR 702.9b, 509.1: flying blocks")) (Text.pack "Flying blocks", Text.pack "CR 702.9b, 509.1")
      Spec.assertEqWith s "uncited" (Replay.titled (Text.pack "x") (Text.pack "Grizzly Bears attacks: it hits")) (Text.pack "Grizzly Bears attacks: it hits", Text.empty)
      Spec.assertEqWith s "the suite's tag" (Replay.titled (Text.pack "x") (Text.pack "CR 305.2a gameplay: Word of Command makes bob play a land")) (Text.pack "Word of Command makes bob play a land", Text.pack "CR 305.2a")
      Spec.assertEqWith s "undescribed" (Replay.titled (Text.pack "game/cr-305-2a-a-land-is-played") Text.empty) (Text.pack "CR 305 2a a land is played", Text.empty)

  Spec.describe s "turnStarts" $ do
    Spec.it s "names the first entry and each entry opening a turn" $ do
      let on turn = Timed.MkTimed (When.MkWhen turn Phase.PrecombatMain (Label.MkLabel (Text.pack "player1")) 0) Nothing (Entry.Do Move.Pass)
      Spec.assertEqWith s "starts" (Replay.turnStarts (fmap on [1, 1, 2, 2, 2, 3, 1])) [0, 2, 5, 6]
      Spec.assertEqWith s "empty" (Replay.turnStarts []) []

  Spec.describe s "lineOf" $ do
    let at turn phase label = When.MkWhen turn phase (Label.MkLabel (Text.pack label)) 0
        answering prompt with = Entry.Do (Move.Answer (Answer.MkAnswer (Text.pack prompt) with False))
        names = Map.fromList [(Text.pack "player1", Text.pack "You"), (Text.pack "player2", Text.pack "Player 2")]
        line asked when entry = Replay.lineOf names True asked (Timed.MkTimed when Nothing entry)
    Spec.it s "names the turn, the step, the seat and what it does" $ do
      Spec.assertEqWith s "chance" (line Nothing (at 1 (Phase.Beginning BeginningStep.Untap) "player1") (answering "Shuffle" (Reply.Array []))) (Text.pack "Turn 1 \x00b7 Untap \x00b7 a library is shuffled")
      Spec.assertEqWith s "before the first turn" (Replay.lineOf names False Nothing (Timed.MkTimed (at 1 (Phase.Beginning BeginningStep.Untap) "player1") Nothing (answering "Shuffle" (Reply.Array [])))) (Text.pack "Before the first turn \x00b7 a library is shuffled")
      Spec.assertEqWith s "a concession" (line Nothing (at 3 Phase.PrecombatMain "player2") (Entry.Do Move.Concede)) (Text.pack "Turn 3 \x00b7 Precombat main \x00b7 Player 2 concedes")
      Spec.assertEqWith s "a seat unnamed" (Replay.lineOf Map.empty True Nothing (Timed.MkTimed (at 3 Phase.PrecombatMain "player2") Nothing (Entry.Do Move.Pass))) (Text.pack "Turn 3 \x00b7 Precombat main \x00b7 player2 passes")
      Spec.assertEqWith s "you" (line Nothing (at 3 Phase.PrecombatMain "player1") (Entry.Do Move.Pass)) (Text.pack "Turn 3 \x00b7 Precombat main \x00b7 You pass")

    Spec.it s "says an answer as the question asked there words it" $ do
      let attacking = at 2 (Phase.Combat CombatStep.DeclareAttackers) "player2"
      Spec.assertEqWith s "an answer" (line (Just (request 1 "DeclareAttackers" (Reply.Object []))) attacking (answering "DeclareAttackers" (Reply.Object []))) (Text.pack "Turn 2 \x00b7 Declare attackers \x00b7 Player 2 declares attackers")
      Spec.assertEqWith s "a scenario's move" (line (Just (request 1 "DeclareAttackers" (Reply.Object []))) attacking (Entry.Do (Move.Attack mempty))) (Text.pack "Turn 2 \x00b7 Declare attackers \x00b7 Player 2 declares attackers")
      Spec.assertEqWith s "an action, by its index" (line (Just (request 1 "ChooseAction" (Reply.Array [Reply.Object [(Text.pack "action", Reply.Text (Text.pack "Pass"))]]))) attacking (answering "ChooseAction" (Reply.Number 0))) (Text.pack "Turn 2 \x00b7 Declare attackers \x00b7 Player 2 passes")

  Spec.describe s "picker" $ do
    Spec.it s "lists the names holding what is typed, whatever its case" $ do
      let view = Replay.picker (Replay.listing []) {Replay.typed = Text.pack "PIKER"} (Just (Map.empty, Map.empty, fmap Text.pack ["goblin-piker-attacks", "shock-kills", "a-piker-blocks"]))
          page = Html.toText view
      Spec.assertEqWith s "match" (Html.labelled (Text.pack "button") (Text.pack "Goblin piker attacks") view >>= \path -> Html.handler (Text.pack "click") path view) (Just (Replay.MkOpen (Text.pack "goblin-piker-attacks")))
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "href=\"?scenario=") page)) "no link"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack "Shock kills") page)) "filtered out"
      Spec.assertBool s (Text.isInfixOf (Text.pack "2 of 3") page) "count"

    Spec.it s "lists the manifest's matching names under their themes ahead of every name" $ do
      let themes = Map.fromList [(Text.pack "Combat", fmap Text.pack ["a-piker-blocks", "shock-kills"]), (Text.pack "Burn", fmap Text.pack ["shock-kills"])]
          page = Html.toText $ Replay.picker (Replay.listing []) {Replay.typed = Text.pack "piker"} (Just (themes, Map.empty, fmap Text.pack ["goblin-piker-attacks", "shock-kills", "a-piker-blocks"]))
          at needle = Text.length (fst (Text.breakOn (Text.pack needle) page))
      Spec.assertBool s (at ">Combat<" < at "Every scenario" && at "Every scenario" < at "goblin-piker-attacks") "the theme first"
      Spec.assertBool s (not (Text.isInfixOf (Text.pack ">Burn<") page)) "a theme with no match left out"
      Spec.assertBool s (at ">Combat<" < at ">a-piker-blocks<") "the theme's match"

    Spec.it s "titles a scenario from its description where it was read, and matches the title" $ do
      let described = Map.fromList [(Text.pack "shock-kills", Text.pack "CR 120.3: Shock deals 2 damage to a bear")]
          view = Replay.picker (Replay.listing []) {Replay.typed = Text.pack "bear"} (Just (Map.empty, described, fmap Text.pack ["goblin-piker-attacks", "shock-kills"]))
      Spec.assertEqWith s "titled" (Html.labelled (Text.pack "button") (Text.pack "Shock deals 2 damage to a bear") view >>= \path -> Html.handler (Text.pack "click") path view) (Just (Replay.MkOpen (Text.pack "shock-kills")))

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

-- | The path to the step button a screen reader names this.
step :: String -> Html.Html msg -> Maybe (Seq.Seq Natural)
step label = Html.search $ \node -> case node of
  Html.MkElement n -> Html.tag n == Text.pack "button" && Html.spoken node == Text.pack label
  Html.MkText _ -> False

-- | A question of this seat's of this kind and offer.
request :: Natural -> String -> Reply.Reply -> Request.Request
request seat kind offer = Request.MkRequest Nothing (Question.MkQuestion (PlayerId.MkPlayerId seat) (Text.pack kind) offer Nothing Nothing) Nothing 0

-- | Two seats, as a scenario labels them.
labelled :: [(PlayerId.PlayerId, Text.Text)]
labelled = [(PlayerId.MkPlayerId 0, Text.pack "alice"), (PlayerId.MkPlayerId 1, Text.pack "bob")]

frame :: Replay.Frame
frame =
  Replay.MkFrame
    { Replay.name = Text.pack "a-scenario",
      Replay.description = Text.pack "what it shows",
      Replay.seats = Replay.named Map.empty labelled,
      Replay.viewer = PlayerId.MkPlayerId 0,
      Replay.position = 0,
      Replay.positions = 3,
      Replay.turns = [0, 2],
      Replay.playing = False,
      Replay.board = Nothing,
      Replay.earlier = [],
      Replay.outcome = Text.empty,
      Replay.depth = 0,
      Replay.entryLine = Nothing,
      Replay.question = Nothing,
      Replay.failure = Nothing,
      Replay.reading = Nothing,
      Replay.written = Map.empty
    }
