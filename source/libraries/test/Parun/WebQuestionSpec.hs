-- Covers Parun.Web.Question and Parun.Web.Board: a line protocol request as a
-- page, and the reply its taps build, from requests the engine makes.
module Parun.WebQuestionSpec where

import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Registry as Registry
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerView as PlayerView.Type
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Question as Question.Web

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "WebQuestion" $ do
  Spec.it s "a priority offering a land shows the board and a button per action, and a tap answers by index" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs) = S.addObjectIn Zone.Hand mountain S.alice (Setup.emptyGame S.bothPlayers)
        request = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass, Action.Play land Nothing])
        view = Question.Web.view request Question.Web.blank
        page = Html.toText view
        tap label = Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
    Spec.assertBool s (Text.pack "<button class=\"primary\" type=\"button\">Pass</button>" `Text.isInfixOf` page) "pass"
    Spec.assertEqWith s "tap" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (tap "Play Mountain")) (Just (Question.Web.MkReplied (R.Number 1)))
    Spec.assertEqWith s "skip" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (tap "End turn")) (Just (Question.Web.MkReplied (R.Number 0)))
    Spec.assertEqWith s "not automatic" (Question.Web.automatic Question.Web.MkStandard request) Nothing
    Spec.assertBool s (Text.pack "aria-keyshortcuts=\"E\">End turn</button>" `Text.isInfixOf` page) "E ends the turn"
    -- The hand's Mountain is lit, since the seat may play it, and tapping it reads it.
    Spec.assertBool s (Text.pack "<section class=\"hand\"" `Text.isInfixOf` page) "hand"
    Spec.assertBool s (Text.pack "class=\"card c-land playable\"" `Text.isInfixOf` page) "playable"
    Spec.assertEqWith s "a card in hand is tapped to read it" (Html.search isCard view >>= \path -> Html.handler (Text.pack "click") path view) (Just (Question.Web.MkRead land))
    -- Reading it opens its sheet, whose button plays it.
    let reading = Question.Web.view request Question.Web.blank {Question.Web.reading = Just (Card.MkReading land [Text.pack "({T}: Add {R}.)"] True)}
        sheet = Html.search (\html -> Text.pack "<section class=\"sheet" `Text.isPrefixOf` Html.toText html) reading
    Spec.assertBool s (Text.pack "<span class=\"ms ms-r glyph\" title=\"{R}\" role=\"img\" aria-label=\"red\">\xe603</span>" `Text.isInfixOf` Html.toText reading) "oracle"
    let action = do
          at <- sheet
          inner <- Html.at at reading
          path <- Html.labelled (Text.pack "button") (Text.pack "Play Mountain") inner
          Html.handler (Text.pack "click") (at <> path) reading
    Spec.assertEqWith s "sheet action" action (Just (Question.Web.MkAnswer (R.Number 1)))

  Spec.it s "CR 602.1a an activated ability is offered by its Oracle text" $ do
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (oid, gs) = S.addPermanent sorcerer S.alice (Setup.emptyGame S.bothPlayers)
        request = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice (Action.Pass : fmap (Action.Activate oid) (Projection.abilitiesOf oid gs)))
    -- The tap symbol is drawn ({T} as a glyph), so the text around it is matched.
    Spec.assertBool s (Text.pack ": Prodigal Sorcerer deals 1 damage to any target." `Text.isInfixOf` Html.content (Question.Web.view request Question.Web.blank)) "labelled"

  Spec.it s "options the seat can't tell apart share one button, which picks the next" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (one, g1) = S.addPermanent bears S.alice (Setup.emptyGame S.bothPlayers)
        (two, gs) = S.addPermanent bears S.alice g1
        request = (requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])) {Request.question = Question.MkQuestion S.alice (Text.pack "DeclareAttackers") (R.Array [PromptOffer.object Protocol.namer one, PromptOffer.object Protocol.namer two])}
        tap picked label = let view = Question.Web.view request Question.Web.blank {Question.Web.selected = picked} in Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
    Spec.assertEqWith s "first" (tap Set.empty "Your Grizzly Bears 2/2 \x00d7\&2") (Just (Question.Web.MkToggle 0 0))
    Spec.assertEqWith s "then the next" (tap (Set.singleton (0, 0)) "1 of Your Grizzly Bears 2/2 \x00d7\&2") (Just (Question.Web.MkToggle 0 1))
    Spec.assertBool s (Text.pack "aria-keyshortcuts=\"A\">All</button>" `Text.isInfixOf` Html.toText (Question.Web.view request Question.Web.blank)) "A attacks with all"

  Spec.it s "the log heads each turn the seat reads into, once" $ do
    let view = PlayerView.viewFor S.alice (Setup.emptyGame S.bothPlayers)
        (marks, first) = Log.unseen Map.empty view
    Spec.assertEqWith s "heading" (take 1 first) [Log.MkTurn (PlayerView.Type.turn view) (PlayerView.Type.active view)]
    Spec.assertEqWith s "read" (snd (Log.unseen marks view)) []
    let event = Log.MkEvent Log.MkMajor . Text.pack
        turn n = Log.MkTurn n S.alice
    let busy = Question.Web.blank {Question.Web.logged = Seq.fromList (fmap (event . show) [1 :: Int .. 6]), Question.Web.fresh = 6}
        asked = requestFor (Setup.emptyGame S.bothPlayers) (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
    Spec.assertBool s (Text.pack "\x2026\&and 2 more in the log" `Text.isInfixOf` Html.content (Question.Web.view asked busy)) "news says how many more"
    let shock = ObjectId.MkObjectId 5
        linked = Log.item S.alice (Just (Map.fromList [(Text.pack "Shock", shock), (Text.pack "Shocking Grasp", ObjectId.MkObjectId 6)], Question.Web.MkRead)) False (event "Player 1 casts Shocking Grasp, then Shock.")
        tapped label = Html.labelled (Text.pack "button") (Text.pack label) linked >>= \path -> Html.handler (Text.pack "click") path linked
    Spec.assertEqWith s "a named card opens its sheet" (tapped "Shock") (Just (Question.Web.MkRead shock))
    Spec.assertEqWith s "the longest name wins" (tapped "Shocking Grasp") (Just (Question.Web.MkRead (ObjectId.MkObjectId 6)))
    Spec.assertEqWith s "newest turn first, its heading on top" (fmap snd (Foldable.toList (Log.newestFirst (Seq.fromList (zip [0 :: Int ..] [event "a", turn 1, event "b", event "c", turn 2, event "d"]))))) [turn 2, event "d", turn 1, event "c", event "b", event "a"]

  Spec.it s "a priority offering only a pass, the hotseat's concession, and play or draw are answered unasked" $ do
    let gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith s "pass" (Question.Web.automatic Question.Web.MkStandard (requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass]))) (Just (R.Number 0))
    Spec.assertEqWith s "concede" (Question.Web.automatic Question.Web.MkStandard (bare (Text.pack "Concede") R.Null)) (Just (R.Text (Text.pack "Continues")))
    Spec.assertEqWith s "play first" (Question.Web.automatic Question.Web.MkStandard (bare (Text.pack "PlayOrDraw") (R.Array [R.Text (Text.pack "Play"), R.Text (Text.pack "Draw")]))) (Just (R.Text (Text.pack "Play")))

  Spec.it s "full control asks a lone pass and who plays first; auto passes an empty stack outside the seat's main phases" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs0) = S.addObjectIn Zone.Hand mountain S.alice (Setup.emptyGame S.bothPlayers)
        at phase = gs0 {GameState.turn = (GameState.turn gs0) {TurnRegisters.phase = phase}}
        offering phase = requestFor (at phase) (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass, Action.Play land Nothing])
        lonePass = requestFor gs0 (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
    Spec.assertEqWith s "full: a lone pass" (Question.Web.automatic Question.Web.MkFull lonePass) Nothing
    Spec.assertEqWith s "full: play first" (Question.Web.automatic Question.Web.MkFull (bare (Text.pack "PlayOrDraw") (R.Array [R.Text (Text.pack "Play"), R.Text (Text.pack "Draw")]))) Nothing
    Spec.assertEqWith s "auto: upkeep" (Question.Web.automatic Question.Web.MkAutoPass (offering (Phase.Beginning BeginningStep.Upkeep))) (Just (R.Number 0))
    Spec.assertEqWith s "auto: main phase" (Question.Web.automatic Question.Web.MkAutoPass (offering Phase.PrecombatMain)) Nothing
    Spec.assertEqWith s "standard: upkeep" (Question.Web.automatic Question.Web.MkStandard (offering (Phase.Beginning BeginningStep.Upkeep))) Nothing
    Spec.assertEqWith s "a covered cost is paid" (Question.Web.paying (bare (Text.pack "ChooseExtraManaSource") R.Null)) (Just R.Null)
    Spec.assertEqWith s "named" (Question.Web.levelNamed "auto") (Just Question.Web.MkAutoPass)

  Spec.it s "a payment shows each source with what it makes, and a tap answers with that source" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs) = S.addPermanent mountain S.alice (Setup.emptyGame S.bothPlayers)
        source = R.Object [(Text.pack "source", PromptOffer.object Protocol.namer land), (Text.pack "makes", R.Array [R.Array [Either.fromRight R.Null (Codec.Reply.fromValue (Codec.encode Codec.ManaType.codec (ManaType.Colored Color.Red)))]])]
        shown = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
        request = shown {Request.question = Question.MkQuestion S.alice (Text.pack "ChooseManaSource") (R.Object [(Text.pack "sources", R.Array [source])])}
        page = Question.Web.view request Question.Web.blank
    Spec.assertBool s (Text.pack "<h2>Choose a source.</h2>" `Text.isInfixOf` Html.toText page) "heading"
    Spec.assertEqWith s "tap" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (Html.labelled (Text.pack "button") (Text.pack "Your Mountain (makes R)") page >>= \path -> Html.handler (Text.pack "click") path page)) (Just (Question.Web.MkReplied (PromptOffer.object Protocol.namer land)))
    -- The Mountain's tile on the battlefield pays too.
    Spec.assertEqWith s "tile" (Html.search isCard page >>= \path -> Html.handler (Text.pack "click") path page) (Just (Question.Web.MkAnswer (PromptOffer.object Protocol.namer land)))

  Spec.it s "a bottom pick toggles cards and answers with exactly the count picked" $ do
    let request = asking (Text.pack "Bottom") (R.Object [(Text.pack "from", R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]), (Text.pack "count", R.Number 1)])
        after = picking request
    Spec.assertEqWith s "too few" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Choose 1 card to put on the bottom of your library: pick 1."))
    Spec.assertEqWith s "picked" (Question.Web.step request (after [Question.Web.MkToggle 0 1]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Text (Text.pack "$2")]))
    Spec.assertEqWith s "unpicked" (after [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 1]) Set.empty

  Spec.it s "CR 508.1a attackers are any of those offered, none included" $ do
    let request = asking (Text.pack "DeclareAttackers") (R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")])
    Spec.assertEqWith s "none" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array []))
    Spec.assertEqWith s "both" (Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]))

  Spec.it s "CR 509.1a blocks pair each blocker picked for with the attackers it blocks" $ do
    let request = asking (Text.pack "DeclareBlockers") (R.Object [(Text.pack "blockers", R.Array [R.Text (Text.pack "$3"), R.Text (Text.pack "$4")]), (Text.pack "attackers", R.Array [R.Text (Text.pack "$1")])])
    Spec.assertEqWith s "one block" (Question.Web.step request (picking request [Question.Web.MkToggle 1 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [R.Text (Text.pack "$4"), R.Array [R.Text (Text.pack "$1")]]]))
    -- On the table, a blocker's tile is tapped first, then the attacker's.
    Spec.assertEqWith s "table" (Question.Web.blocks request) (Just ([ObjectId.MkObjectId 3, ObjectId.MkObjectId 4], [ObjectId.MkObjectId 1]))
    Spec.assertEqWith s "focus keeps picks" (Question.Web.step request (Set.singleton (1, 0)) (Question.Web.MkFocus 0) Text.empty) (Question.Web.MkPicking (Set.singleton (1, 0)))

  Spec.it s "CR 601.2c targets are exactly each slot's count, sent by slot" $ do
    let slot = R.Text (Text.pack "target")
        request = asking (Text.pack "ChooseTargets") (R.Object [(Text.pack "source", R.Text (Text.pack "$9")), (Text.pack "slots", R.Array [R.Array [slot, R.Array [R.Number 1, R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]]]])])
    Spec.assertEqWith s "too few" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Choose 1 target for object 9 (target): pick 1."))
    Spec.assertEqWith s "one" (Question.Web.step request (picking request [Question.Web.MkToggle 0 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [slot, R.Array [R.Text (Text.pack "$1")]]]))

  Spec.it s "CR 603.3b triggered abilities are ordered by tapping each in turn, answered once all are" $ do
    let request = asking (Text.pack "OrderTriggers") (R.Array [R.Object [(Text.pack "source", R.Text (Text.pack "$1"))], R.Object [(Text.pack "source", R.Text (Text.pack "$2"))]])
    Spec.assertEqWith s "first" (Question.Web.step request Set.empty (Question.Web.MkToggle 0 1) Text.empty) (Question.Web.MkPicking (Set.singleton (0, 1)))
    Spec.assertEqWith s "last" (Question.Web.step request (Set.singleton (0, 1)) (Question.Web.MkToggle 0 0) Text.empty) (Question.Web.MkReplied (R.Array [R.Number 1, R.Number 0]))
    Spec.assertEqWith s "again" (Question.Web.step request (Set.singleton (0, 1)) (Question.Web.MkToggle 0 1) Text.empty) (Question.Web.MkPicking (Set.singleton (0, 1)))

  Spec.it s "CR 510.1c combat damage goes to a lone recipient unasked, and among several adds up to the total" $ do
    let offer recipients = R.Object [(Text.pack "source", R.Text (Text.pack "$9")), (Text.pack "lethal", R.Array [R.Array [r, R.Number 1] | r <- recipients]), (Text.pack "paid", R.Array []), (Text.pack "total", R.Number 3)]
        one = R.Text (Text.pack "$1")
        two = R.Text (Text.pack "$2")
        request = asking (Text.pack "AssignCombatDamage") (offer [one, two])
    Spec.assertEqWith s "lone" (Question.Web.automatic Question.Web.MkStandard (asking (Text.pack "AssignCombatDamage") (offer [one]))) (Just (R.Array [R.Array [one, R.Number 3]]))
    Spec.assertEqWith s "several" (Question.Web.automatic Question.Web.MkStandard request) Nothing
    Spec.assertEqWith s "adds up" (Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 1 2]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [one, R.Number 1], R.Array [two, R.Number 2]]))
    Spec.assertBool s (case Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 1 1]) Question.Web.MkSubmit Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "short"
    Spec.assertEqWith s "a pick replaces the row's last" (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 3]) (Set.singleton (0, 3))
    -- The form starts lethal to the first and the rest to the last, which answers as it stands.
    Spec.assertEqWith s "default" (Question.Web.initial request) (Set.fromList [(0, 1), (1, 2)])
    Spec.assertEqWith s "default answers" (Question.Web.step request (Question.Web.initial request) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [one, R.Number 1], R.Array [two, R.Number 2]]))

  Spec.it s "CR 701.23b a search finds up to its count, none included" $ do
    let request = asking (Text.pack "Search") (R.Object [(Text.pack "from", R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]), (Text.pack "names", R.Array [R.Text (Text.pack "Forest"), R.Text (Text.pack "Plains")]), (Text.pack "count", R.Number 1)])
    Spec.assertBool s (Text.pack "<button type=\"button\">Forest</button><button type=\"button\">Plains</button><button class=\"primary\" type=\"button\">Find</button>" `Text.isInfixOf` Html.toText (Question.Web.view request Question.Web.blank)) "names"
    Spec.assertEqWith s "none" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array []))
    Spec.assertBool s (case Question.Web.step request (picking request [Question.Web.MkToggle 0 0, Question.Web.MkToggle 0 1]) Question.Web.MkSubmit Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "too many"

  Spec.it s "CR 701.22a a scry places each card on top or bottom in the order tapped, sent as the bottom pile then the top" $ do
    let one = R.Text (Text.pack "$1")
        two = R.Text (Text.pack "$2")
        request = asking (Text.pack "ChooseScry") (R.Object [(Text.pack "from", R.Array [one, two])])
    Spec.assertEqWith s "placed" (Question.Web.step request (picking request [Question.Web.MkToggle 1 0]) (Question.Web.MkToggle 0 1) Text.empty) (Question.Web.MkReplied (R.Array [R.Array [one], R.Array [two]]))
    Spec.assertEqWith s "half" (Question.Web.step request Set.empty (Question.Web.MkToggle 0 1) Text.empty) (Question.Web.MkPicking (Set.singleton (0, 1)))

  Spec.it s "an offer listing an order is ordered by tapping, an offer listing options is one tap, and a range is a number" $ do
    let ordered = asking (Text.pack "OrderTimestamps") (R.Object [(Text.pack "order", R.Array [R.Text (Text.pack "a"), R.Text (Text.pack "b")])])
        listed = asking (Text.pack "ChooseWhatever") (R.Object [(Text.pack "options", R.Array [R.Text (Text.pack "Up"), R.Text (Text.pack "Down")])])
        ranged = asking (Text.pack "ChooseNumber") (R.Object [(Text.pack "least", R.Number 1), (Text.pack "greatest", R.Number 3)])
    Spec.assertEqWith s "ordered" (Question.Web.step ordered (Set.singleton (0, 1)) (Question.Web.MkToggle 0 0) Text.empty) (Question.Web.MkReplied (R.Array [R.Number 1, R.Number 0]))
    Spec.assertBool s (Text.pack "<button type=\"button\">Up</button><button type=\"button\">Down</button>" `Text.isInfixOf` Html.toText (Question.Web.view listed Question.Web.blank)) "listed"
    Spec.assertBool s (Text.pack "<button type=\"button\">1</button><button type=\"button\">2</button><button type=\"button\">3</button>" `Text.isInfixOf` Html.toText (Question.Web.view ranged Question.Web.blank)) "ranged"

  Spec.it s "a card name is typed as itself" $ do
    let request = asking (Text.pack "ChooseCardName") R.Null
    Spec.assertEqWith s "named" (Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack " Grizzly Bears ")) (Question.Web.MkReplied (R.Text (Text.pack "Grizzly Bears")))
    Spec.assertBool s (case Question.Web.step request Set.empty Question.Web.MkTyped Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "empty"

  Spec.it s "any other question shows its offer and reads a reply typed as JSON" $ do
    let request = bare (Text.pack "ChooseX") (R.Object [(Text.pack "least", R.Number 0)])
    Spec.assertBool s (Text.pack "<pre>{&quot;least&quot;:0}</pre>" `Text.isInfixOf` Html.toText (Question.Web.view request Question.Web.blank)) "offer"
    Spec.assertEqWith s "read" (Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack "3")) (Question.Web.MkReplied (R.Number 3))
    Spec.assertBool s (case Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack "{") of Question.Web.MkRefused _ -> True; _ -> False) "refused"

-- | Whether a node is a card's tile.
isCard :: Html.Html msg -> Bool
isCard html = Text.pack "<button class=\"card " `Text.isPrefixOf` Html.toText html

-- | The request the line protocol sends for a prompt, with the deciding seat's view.
requestFor :: GameState.GameState -> Prompt.Prompt r -> Request.Request
requestFor gs prompt = case Protocol.questionOf gs prompt of
  Just question -> Request.MkRequest (Just (PlayerView.viewFor (Question.seat question) gs)) question Nothing
  Nothing -> bare (Text.pack "unasked") R.Null

-- | A request with no view, for questions whose form reads only the offer.
bare :: Text.Text -> R.Reply -> Request.Request
bare kind offer = Request.MkRequest Nothing (Question.MkQuestion S.alice kind offer) Nothing

-- | A request asking the kind with the offer, with a view of an empty game.
asking :: Text.Text -> R.Reply -> Request.Request
asking kind offer = (requestFor (Setup.emptyGame S.bothPlayers) (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])) {Request.question = Question.MkQuestion S.alice kind offer}

-- | The picks a run of events leaves.
picking :: Request.Request -> [Question.Web.Event] -> Set.Set (Natural, Natural)
picking request = Foldable.foldl' (\picked event -> case Question.Web.step request picked event Text.empty of Question.Web.MkPicking now -> now; _ -> picked) Set.empty
