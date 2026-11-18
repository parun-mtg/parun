-- | A view as a tree: what the browser shows, and the message each event
-- handler in it sends. Pure, so a view is built and tested natively.
module Parun.Web.Html where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Web.Attribute as Attribute

-- | A node of a view.
data Html msg
  = -- | An element.
    MkElement (Node msg)
  | -- | A text node.
    MkText Text.Text
  deriving (Eq, Show)

-- | An element: its tag, its attributes and its children.
data Node msg = MkNode
  { tag :: Text.Text,
    attributes :: Seq.Seq (Attribute.Attribute msg),
    children :: Children msg
  }
  deriving (Eq, Show)

-- | An element's children.
data Children msg
  = -- | Matched to the previous view's by position.
    MkUnkeyed (Seq.Seq (Html msg))
  | -- | Matched to the previous view's by key, so a child that moves keeps its
    -- DOM node (and with it focus and typed text).
    MkKeyed (Seq.Seq (Text.Text, Html msg))
  deriving (Eq, Show)

instance Functor Html where
  fmap f html = case html of
    MkElement node -> MkElement $ fmap f node
    MkText t -> MkText t

instance Functor Node where
  fmap f node =
    MkNode
      { tag = tag node,
        attributes = fmap (fmap f) $ attributes node,
        children = fmap f $ children node
      }

instance Functor Children where
  fmap f kids = case kids of
    MkUnkeyed items -> MkUnkeyed $ fmap (fmap f) items
    MkKeyed items -> MkKeyed $ fmap (Bifunctor.second $ fmap f) items

element :: Text.Text -> [Attribute.Attribute msg] -> [Html msg] -> Html msg
element name attrs kids = MkElement . MkNode name (Seq.fromList attrs) . MkUnkeyed $ Seq.fromList kids

keyed :: Text.Text -> [Attribute.Attribute msg] -> [(Text.Text, Html msg)] -> Html msg
keyed name attrs kids = MkElement . MkNode name (Seq.fromList attrs) . MkKeyed $ Seq.fromList kids

text :: Text.Text -> Html msg
text = MkText

-- | An element's children in order, keyed or not.
childList :: Children msg -> Seq.Seq (Html msg)
childList kids = case kids of
  MkUnkeyed items -> items
  MkKeyed items -> fmap snd items

-- | The node a path of child indices leads to from the root, if any.
at :: Seq.Seq Natural -> Html msg -> Maybe (Html msg)
at path html = case Seq.viewl path of
  Seq.EmptyL -> Just html
  i Seq.:< rest -> case html of
    MkText _ -> Nothing
    MkElement node -> do
      n <- Natural.toInt i
      child <- Seq.lookup n . childList $ children node
      at rest child

-- | The message an event of this type sends when it happens at the end of the
-- path: the handler of the deepest node on the path that has one, as the
-- event bubbles. Nothing where no node handles it, or the path leads nowhere.
handler :: Text.Text -> Seq.Seq Natural -> Html msg -> Maybe msg
handler event path html = Monad.join $ along event path html

-- | handler, or Nothing where the path leads nowhere.
along :: Text.Text -> Seq.Seq Natural -> Html msg -> Maybe (Maybe msg)
along event path html = case html of
  MkText _ -> if Seq.null path then Just Nothing else Nothing
  MkElement node ->
    let own = Foldable.foldl' (\found attribute -> case attribute of Attribute.MkOn name msg | name == event -> Just msg; _ -> found) Nothing $ attributes node
     in case Seq.viewl path of
          Seq.EmptyL -> Just own
          i Seq.:< rest -> do
            n <- Natural.toInt i
            child <- Seq.lookup n . childList $ children node
            below <- along event rest child
            Just $ below <|> own

-- | The view as HTML source, escaped; event handlers are left out, since the
-- page dispatches events itself.
toText :: Html msg -> Text.Text
toText html = case html of
  MkText t -> escape t
  MkElement node ->
    let open = Text.pack "<" <> tag node <> foldMap property (attributes node) <> Text.pack ">"
        property attribute = case attribute of
          Attribute.MkProperty name value -> Text.pack " " <> name <> Text.pack "=\"" <> escape value <> Text.pack "\""
          Attribute.MkOn _ _ -> Text.empty
        close = Text.pack "</" <> tag node <> Text.pack ">"
     in open <> foldMap toText (childList $ children node) <> close

escape :: Text.Text -> Text.Text
escape = Text.concatMap $ \c -> case c of
  '&' -> Text.pack "&amp;"
  '<' -> Text.pack "&lt;"
  '>' -> Text.pack "&gt;"
  '"' -> Text.pack "&quot;"
  _ -> Text.singleton c

-- | The text a node shows: its own, or its descendants' in order.
content :: Html msg -> Text.Text
content html = case html of
  MkText t -> t
  MkElement node -> foldMap content (childList (children node))

-- | The path to the first node, in document order, that the predicate holds of.
search :: (Html msg -> Bool) -> Html msg -> Maybe (Seq.Seq Natural)
search wanted html
  | wanted html = Just Seq.empty
  | otherwise = case html of
      MkText _ -> Nothing
      MkElement node -> Foldable.asum (Seq.mapWithIndex (\i child -> fmap (Int.toNaturalSaturating i Seq.<|) (search wanted child)) (childList (children node)))

-- | The path to the first element with this tag whose text is this.
labelled :: Text.Text -> Text.Text -> Html msg -> Maybe (Seq.Seq Natural)
labelled name label = search $ \html -> case html of
  MkElement node -> tag node == name && content html == label
  MkText _ -> False
