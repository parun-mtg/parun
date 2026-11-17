-- Covers Parun.Web.Question and Parun.Web.Board: a line protocol request as a
-- page, and the reply its taps build, from requests the engine makes.
module Parun.WebQuestionSpec where

import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Setup as Setup
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Registry as Registry
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Zone as Zone
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Question as Question.Web

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "WebQuestion" $ do
  Spec.it s "a priority offering a land shows the board and a button per action, and a tap answers by index" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs) = S.addObjectIn Zone.Hand mountain S.alice (Setup.emptyGame S.bothPlayers)
        request = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass, Action.Play land Nothing])
        page = Html.toText $ Question.Web.view request Set.empty
    Spec.assertBool s (Text.pack "<button>Pass</button><button>Play Mountain</button>" `Text.isInfixOf` page) "buttons"
    Spec.assertBool s (Text.pack "<p>Your hand:</p><ul><li>Mountain</li></ul>" `Text.isInfixOf` page) "hand"
    -- The form follows the board and its heading: the second button is the main element's fourth child.
    Spec.assertEqWith s "tap" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (Html.handler (Text.pack "click") (Seq.fromList [3]) (Question.Web.view request Set.empty))) (Just (Question.Web.MkReplied (R.Number 1)))
    Spec.assertEqWith s "not automatic" (Question.Web.automatic request) Nothing

  Spec.it s "a priority offering only a pass, and the hotseat's concession, are answered unasked" $ do
    let gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith s "pass" (Question.Web.automatic (requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass]))) (Just (R.Number 0))
    Spec.assertEqWith s "concede" (Question.Web.automatic (bare (Text.pack "Concede") R.Null)) (Just (R.Text (Text.pack "Continues")))

  Spec.it s "a payment shows each source with what it makes, and a tap answers with that source" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs) = S.addPermanent mountain S.alice (Setup.emptyGame S.bothPlayers)
        source = R.Object [(Text.pack "source", PromptOffer.object Protocol.namer land), (Text.pack "makes", R.Array [R.Array [Either.fromRight R.Null (Codec.Reply.fromValue (Codec.encode Codec.ManaType.codec (ManaType.Colored Color.Red)))]])]
        shown = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
        request = shown {Request.question = Question.MkQuestion S.alice (Text.pack "ChooseManaSource") (R.Object [(Text.pack "sources", R.Array [source])])}
        page = Question.Web.view request Set.empty
    Spec.assertBool s (Text.pack "<h2>Choose a source.</h2><button>Your Mountain (makes R)</button><button>Stop paying</button>" `Text.isInfixOf` Html.toText page) "buttons"
    Spec.assertEqWith s "tap" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (Html.handler (Text.pack "click") (Seq.fromList [2]) page)) (Just (Question.Web.MkReplied (PromptOffer.object Protocol.namer land)))

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
    Spec.assertEqWith s "lone" (Question.Web.automatic (asking (Text.pack "AssignCombatDamage") (offer [one]))) (Just (R.Array [R.Array [one, R.Number 3]]))
    Spec.assertEqWith s "several" (Question.Web.automatic request) Nothing
    Spec.assertEqWith s "adds up" (Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 1 2]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [one, R.Number 1], R.Array [two, R.Number 2]]))
    Spec.assertBool s (case Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 1 1]) Question.Web.MkSubmit Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "short"
    Spec.assertEqWith s "a pick replaces the row's last" (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 3]) (Set.singleton (0, 3))

  Spec.it s "CR 701.23b a search finds up to its count, none included" $ do
    let request = asking (Text.pack "Search") (R.Object [(Text.pack "from", R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]), (Text.pack "names", R.Array [R.Text (Text.pack "Forest"), R.Text (Text.pack "Plains")]), (Text.pack "count", R.Number 1)])
    Spec.assertBool s (Text.pack "<button>Forest</button><button>Plains</button><button>Find</button>" `Text.isInfixOf` Html.toText (Question.Web.view request Set.empty)) "names"
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
    Spec.assertBool s (Text.pack "<button>Up</button><button>Down</button>" `Text.isInfixOf` Html.toText (Question.Web.view listed Set.empty)) "listed"
    Spec.assertBool s (Text.pack "<button>1</button><button>2</button><button>3</button>" `Text.isInfixOf` Html.toText (Question.Web.view ranged Set.empty)) "ranged"

  Spec.it s "a card name is typed as itself" $ do
    let request = asking (Text.pack "ChooseCardName") R.Null
    Spec.assertEqWith s "named" (Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack " Grizzly Bears ")) (Question.Web.MkReplied (R.Text (Text.pack "Grizzly Bears")))
    Spec.assertBool s (case Question.Web.step request Set.empty Question.Web.MkTyped Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "empty"

  Spec.it s "any other question shows its offer and reads a reply typed as JSON" $ do
    let request = bare (Text.pack "ChooseX") (R.Object [(Text.pack "least", R.Number 0)])
    Spec.assertBool s (Text.pack "<pre>{&quot;least&quot;:0}</pre>" `Text.isInfixOf` Html.toText (Question.Web.view request Set.empty)) "offer"
    Spec.assertEqWith s "read" (Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack "3")) (Question.Web.MkReplied (R.Number 3))
    Spec.assertBool s (case Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack "{") of Question.Web.MkRefused _ -> True; _ -> False) "refused"

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
