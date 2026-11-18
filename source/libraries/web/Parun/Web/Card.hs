-- | A card as the board draws it: a tile coloured by the object's colours,
-- with its name, cost, type line, power and toughness and status, and the
-- sheet a tapped tile opens, with its Oracle text and what the seat may do
-- with it.
module Parun.Web.Card where

import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Play.Render as Render
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Defense as Defense
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Loyalty as Loyalty
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.TapState as TapState
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana

-- | How a tile looks beyond the object itself, and what tapping it sends.
data Cue msg = MkCue
  { -- | Extra classes: "playable", "choosable", "chosen".
    marks :: [String],
    tap :: msg
  }
  deriving (Eq, Show)

-- | An object's tile.
tile :: PlayerView.PlayerView -> Cue msg -> ObjectView.ObjectView -> Html.Html msg
tile view cue o = case ObjectView.characteristics o of
  Nothing | Maybe.isNothing (ObjectView.face o) -> Element.button (["card", "back"] <> marks cue <> tapped o) (tap cue) [Element.el "span" ["name"] [] [Element.string "Hidden card"]]
  _ ->
    Element.button
      (["card", colour o] <> marks cue <> tapped o)
      (tap cue)
      [ Element.el "span" ["top"] [] $ [Element.el "span" ["name"] [] [Element.text (Render.nameOf view o)]] <> foldMap (pure . Mana.cost) (ObjectView.characteristics o >>= ProjectedCharacteristics.manaCost),
        Element.el "span" ["type"] [] [Element.text (typeLine o)],
        Element.el "span" ["foot"] [] (badges o <> stats o)
      ]

tapped :: ObjectView.ObjectView -> [String]
tapped o = ["tapped" | ObjectView.tapped o == TapState.Tapped]

-- | The colour class: one per colour, "multi" for two or more, and for
-- none "land" or "colorless".
colour :: ObjectView.ObjectView -> String
colour o =
  let colours = foldMap (Set.toList . ProjectedCharacteristics.colors) (ObjectView.characteristics o)
   in case colours of
        [c] -> "c-" <> letter c
        [] | Render.isA CardType.Land o -> "c-land"
        [] -> "c-none"
        _ -> "c-multi"

letter :: Color.Color -> String
letter c = case c of
  Color.White -> "w"
  Color.Blue -> "u"
  Color.Black -> "b"
  Color.Red -> "r"
  Color.Green -> "g"

-- | The type line, short: subtypes where there are any, else card types.
typeLine :: ObjectView.ObjectView -> Text.Text
typeLine o = case ObjectView.ability o of
  Just _ -> Text.pack "Ability"
  Nothing -> case ObjectView.characteristics o of
    Nothing -> Text.pack "Face down"
    Just c ->
      let subtypes = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.subtypes c))
          types = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.cardTypes c))
          named = Set.map CardName.unwrap (ProjectedCharacteristics.names c)
       in -- A basic land's subtype is its name, so its card types say more.
          if null subtypes || Set.member (Text.unwords subtypes) named then Text.unwords types else Text.unwords subtypes

-- | Damage marked and counters, each a small badge.
badges :: ObjectView.ObjectView -> [Html.Html msg]
badges o =
  [Element.el "span" ["badge", "damage"] [Element.attr "title" (Text.pack "Damage")] [Element.string (show (ObjectView.damage o) <> " dmg")] | ObjectView.damage o > 0]
    <> [Element.el "span" ["badge", "counter"] [] [Element.text (Text.pack (show n <> " ") <> Render.counterName kind)] | (kind, n) <- Map.toList (ObjectView.counters o), n > 0]

-- | Power and toughness, loyalty or defense, as printed in the corner.
stats :: ObjectView.ObjectView -> [Html.Html msg]
stats o = case ObjectView.characteristics o of
  Nothing -> []
  Just c ->
    foldMap (\(p, t) -> [Element.el "span" ["pt"] [] [Element.string (show p <> "/" <> show t)]]) ((,) <$> ProjectedCharacteristics.power c <*> ProjectedCharacteristics.toughness c)
      <> foldMap (\l -> [Element.el "span" ["pt", "loyalty"] [] [Element.string (loyalty l)]]) (ProjectedCharacteristics.loyalty c)
      <> foldMap (\d -> [Element.el "span" ["pt", "defense"] [] [Element.string (show (Defense.unwrap d))]]) (ProjectedCharacteristics.defense c)
  where
    loyalty l = case l of
      Loyalty.Literal n -> show n
      Loyalty.Variable -> "X"

-- | The printed Oracle text of the faces the seat may read the object by
-- (Render.cardNames), each line apart, a face's name heading its own where
-- there are several; or why the card data could not say.
oracle :: (CardName.CardName -> Either e (Maybe Card.Card)) -> ObjectView.ObjectView -> Either e [Text.Text]
oracle lookUp o = do
  let names = Render.cardNames o
  looked <- traverse lookUp (Set.toList names)
  let faces = concatMap (filter (\f -> Set.member (Face.name f) names) . Foldable.toList . Card.faces) (ListUtils.nubOrd (Maybe.catMaybes looked))
      lines' f = [CardName.unwrap (Face.name f) <> Text.pack ":" | length faces > 1] <> foldMap Text.lines (Face.oracleText f)
  pure (concatMap lines' faces)

-- | What the sheet shows of the object it reads.
data Reading = MkReading
  { object :: ObjectId.ObjectId,
    -- | Its Oracle text, a line each (oracle), or why there is none.
    text :: [Text.Text]
  }
  deriving (Eq, Show)

-- | The sheet a tapped tile opens: the object large, its full type line,
-- status and Oracle text, and these buttons, the seat's options for it.
sheet :: PlayerView.PlayerView -> ObjectView.ObjectView -> Reading -> [Html.Html msg] -> msg -> Html.Html msg
sheet view o reading actions close =
  Element.el
    "div"
    ["sheet-backdrop"]
    [Element.attr "role" (Text.pack "dialog"), Element.attr "aria-modal" (Text.pack "true")]
    [ Element.button ["scrim"] close [Element.el "span" ["sr-only"] [] [Element.string "Close"]],
      Element.el
        "section"
        ["sheet", colour o]
        []
        $ [ Element.el "header" [] [] $
              [Element.el "h2" [] [] [Element.text (Render.nameOf view o)]]
                <> foldMap (pure . Mana.cost) (ObjectView.characteristics o >>= ProjectedCharacteristics.manaCost),
            Element.el "p" ["type-line"] [] [Element.text (fullType o)]
          ]
          <> [Element.el "p" ["status"] [] [Element.text line] | line <- status view o]
          <> [Element.el "div" ["oracle"] [] [Element.el "p" [] [] (Mana.rich line) | line <- text reading, not (Text.null line)] | not (null (text reading))]
          <> [Element.el "div" ["stats-row"] [] (badges o <> stats o) | not (null (badges o <> stats o))]
          <> [Element.el "div" ["sheet-actions"] [] actions | not (null actions)]
          <> [Element.button ["close"] close [Element.string "Close"]]
    ]

-- | The full type line: supertypes, card types, a dash, then subtypes.
fullType :: ObjectView.ObjectView -> Text.Text
fullType o = case (ObjectView.ability o, ObjectView.characteristics o) of
  (Just _, _) -> Text.pack "Ability on the stack"
  (Nothing, Nothing) -> Text.pack "Face-down card"
  (Nothing, Just c) ->
    let supertypes = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.supertypes c))
        types = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.cardTypes c))
        subtypes = fmap (Text.pack . show) (Set.toList (ProjectedCharacteristics.subtypes c))
     in Text.unwords (supertypes <> types <> if null subtypes then [] else Text.pack "\8212" : subtypes)

-- | What the tile cannot fit: tapped, what it is attached to, what an
-- ability says and what it targets, and the card behind a face-down one.
status :: PlayerView.PlayerView -> ObjectView.ObjectView -> [Text.Text]
status view o =
  [Text.pack "Tapped" | ObjectView.tapped o == TapState.Tapped]
    <> [Text.pack "Attached to " <> Render.recipientName view r | r <- Maybe.maybeToList (ObjectView.attachedTo o)]
    <> [Text.pack "\"" <> t <> Text.pack "\"" | t <- Maybe.maybeToList (ObjectView.ability o >>= Render.abilityText view)]
    <> [Text.pack "Targeting " <> Render.listing (fmap (Render.recipientName view) (ObjectView.targets o)) | not (null (ObjectView.targets o))]
    <> [Text.pack "Face down; you may look at it: " <> Text.intercalate (Text.pack " // ") (fmap CardName.unwrap (Set.toList (ProjectedCharacteristics.names f))) | f <- Maybe.maybeToList (ObjectView.face o)]
