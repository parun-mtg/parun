-- | A line protocol request as a page: the seat's board, what it is asked,
-- and an answer built from taps. Each kind of question a Portal game reaches
-- first has its own form; any other shows its offer and takes a reply typed
-- as JSON.
module Parun.Web.Question where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.ManaCost as Codec.ManaCost
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Offer as Offer
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Board as Board
import qualified Parun.Web.Html as Html

-- | What a tap or a typed line does to the answer under way.
data Event
  = -- | Answers with this reply.
    MkAnswer R.Reply
  | -- | Picks the option at this index, or drops it if picked.
    MkToggle Natural
  | -- | Answers with the options picked.
    MkSubmit
  | -- | Answers with the reply typed as JSON into the element.
    MkTyped
  | -- | The seat has the screen, after a hand-off.
    MkReady
  deriving (Eq, Show)

-- | Where an answer stands after an event.
data Outcome
  = -- | Still picking, these options so far.
    MkPicking (Set.Set Natural)
  | -- | Answered.
    MkReplied R.Reply
  | -- | Not answered, and why.
    MkRefused Text.Text
  deriving (Eq, Show)

-- | A reply the seat is never asked for: the hotseat never concedes unasked
-- (CR 104.3a), and a priority offering nothing but a pass and mana abilities
-- is passed (Offer.onlyPass).
automatic :: Request.Request -> Maybe R.Reply
automatic request = case Text.unpack (Question.kind question) of
  "Concede" -> Just . R.Text $ Text.pack "Continues"
  "ChooseAction" -> R.Number <$> Offer.onlyPass (Question.offer question)
  _ -> Nothing
  where
    question = Request.question request

-- | The page for a request, with the options picked so far.
view :: Request.Request -> Set.Set Natural -> Html.Html Event
view request picked =
  Html.element
    (Text.pack "main")
    []
    $ foldMap (\board -> [Board.view board]) (Request.view request)
      <> [Html.element (Text.pack "p") [Attribute.MkProperty (Text.pack "class") (Text.pack "problem")] [Html.text problem] | problem <- Maybe.maybeToList (Request.problem request)]
      <> form request picked

-- | Before a seat's question when the screen last showed another seat's.
handOff :: PlayerId.PlayerId -> Html.Html Event
handOff seat =
  Html.element
    (Text.pack "main")
    []
    [ Html.element (Text.pack "p") [] [Html.text $ Text.pack "Pass the screen to " <> Render.player seat <> Text.pack "."],
      button MkReady $ Text.pack "Ready"
    ]

-- | What an event does, the options picked so far being `picked`.
step :: Request.Request -> Set.Set Natural -> Event -> Text.Text -> Outcome
step request picked event typed = case event of
  MkAnswer reply -> MkReplied reply
  MkToggle i -> MkPicking $ if Set.member i picked then Set.delete i picked else Set.insert i picked
  MkSubmit -> case picks request of
    Just (from, wanted)
      | Natural.length (Set.toList picked) == wanted -> MkReplied $ R.Array [option | (i, option) <- zip [0 ..] from, Set.member i picked]
      | otherwise -> MkRefused . Text.pack $ "Choose " <> show wanted <> "."
    Nothing -> MkPicking picked
  MkTyped -> either (MkRefused . (Text.pack "That reply does not read: " <>)) MkReplied $ Common.parse typed >>= Codec.Reply.fromValue
  MkReady -> MkPicking picked

-- | The cards a pick-so-many question lists, and how many it wants.
picks :: Request.Request -> Maybe ([R.Reply], Natural)
picks request
  | Question.kind question `elem` fmap Text.pack ["Bottom", "ChooseDiscard"] = do
      from <- Protocol.field "from" offer >>= Protocol.arrayOf
      wanted <- Protocol.field "count" offer >>= Protocol.naturalOf
      pure (from, min wanted $ Natural.length from)
  | otherwise = Nothing
  where
    question = Request.question request
    offer = Question.offer question

form :: Request.Request -> Set.Set Natural -> [Html.Html Event]
form request picked = case Text.unpack (Question.kind question) of
  "PlayOrDraw" -> heading "Play first or draw?" : [button (MkAnswer option) (Offer.tagName word) | option@(R.Text word) <- Foldable.fold (Protocol.arrayOf offer)]
  "DeclareMulligan" ->
    heading ("Keep this hand, or mulligan?" <> foldMap (\n -> " A mulligan puts " <> Text.unpack (Render.count n "card") <> " on the bottom.") (Protocol.field "bottom" offer >>= Protocol.naturalOf))
      : [button (MkAnswer . R.Text $ Text.pack word) (Text.pack word) | word <- ["Keep", "Mulligan"]]
  "ChooseAction" | Just shown <- Request.view request, Just options <- Protocol.arrayOf offer -> heading "Choose an action." : [button (MkAnswer $ R.Number i) (Offer.actionName shown option) | (i, option) <- Offer.distinct shown options]
  kind
    | kind `elem` ["ChooseManaSource", "ChooseExtraManaSource"],
      Just shown <- Request.view request,
      Just sources <- Protocol.field "sources" offer >>= Protocol.arrayOf >>= traverse Offer.manaSource ->
        Html.element (Text.pack "h2") [] [Html.text $ foldMap (\cost -> Text.pack "Paying " <> Render.manaCost cost <> Text.pack ". ") (Protocol.field "cost" offer >>= Offer.decoded Codec.ManaCost.codec) <> Text.pack (if kind == "ChooseManaSource" then "Choose a source." else "The cost is covered. Activate another source, or pay now.")]
          : [button (MkAnswer source) (Offer.listedName shown source <> makes yields) | (source, yields) <- sources]
            <> [button (MkAnswer R.Null) $ Text.pack (if kind == "ChooseManaSource" then "Stop paying" else "Pay now")]
  _
    | Just shown <- Request.view request,
      Just (from, wanted) <- picks request ->
        heading ("Choose " <> Text.unpack (Render.count wanted "card") <> (if Question.kind question == Text.pack "Bottom" then " to put on the bottom of your library." else " to discard."))
          : [toggle i (Offer.listedName shown option) | (i, option) <- zip [0 ..] from]
            <> [button MkSubmit $ Text.pack "Done"]
  _ ->
    [ heading (Text.unpack (Render.spaced (Question.kind question)) <> ":"),
      Html.element (Text.pack "pre") [] [Html.text . Common.render $ Codec.Reply.toValue offer],
      Html.element (Text.pack "input") [Attribute.MkProperty (Text.pack "placeholder") (Text.pack "Reply as JSON"), Attribute.MkOn (Text.pack "change") MkTyped] []
    ]
  where
    question = Request.question request
    offer = Question.offer question
    toggle i label = button (MkToggle i) $ (if Set.member i picked then Text.pack "✓ " else Text.empty) <> label
    makes yields = if null yields then Text.empty else Text.pack " (makes " <> Text.intercalate (Text.pack " or ") (fmap (foldMap Render.manaType) yields) <> Text.pack ")"
    heading t = Html.element (Text.pack "h2") [] [Html.text $ Text.pack t]

button :: Event -> Text.Text -> Html.Html Event
button event label = Html.element (Text.pack "button") [Attribute.MkOn (Text.pack "click") event] [Html.text label]
