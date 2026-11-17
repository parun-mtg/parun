-- | What crosses between Haskell and the page's script: patches out as JSON,
-- an event's path in as dotted child indices. Pure, so tested natively; the
-- page's half is @source/web/parun.js@.
module Parun.Web.Wire where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Builder as Builder
import qualified Parun.Json.Value as Value
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch
import qualified Text.Read as Read

-- | The patches as a JSON array, in order.
patches :: Seq.Seq (Patch.Patch msg) -> String
patches = Builder.toString . Value.encode . Value.array . fmap patch . Foldable.toList

patch :: Patch.Patch msg -> Value.Value
patch p = case p of
  Patch.MkReplace at node -> op "replace" at [Value.pair "node" $ html node]
  Patch.MkSetProperty at name value -> op "set" at [Value.pair "name" $ Value.text name, Value.pair "value" $ Value.text value]
  Patch.MkRemoveProperty at name -> op "remove" at [Value.pair "name" $ Value.text name]
  Patch.MkAppend at nodes -> op "append" at [Value.pair "nodes" . Value.array . fmap html $ Foldable.toList nodes]
  Patch.MkTruncate at count -> op "truncate" at [Value.pair "count" . Value.integer $ toInteger count]
  Patch.MkArrange at sources -> op "arrange" at [Value.pair "sources" . Value.array . fmap source $ Foldable.toList sources]
  where
    op name at rest = Value.object $ Value.pair "op" (Value.string name) : Value.pair "path" (path at) : rest

path :: Seq.Seq Natural -> Value.Value
path = Value.array . fmap (Value.integer . toInteger) . Foldable.toList

source :: Patch.Source msg -> Value.Value
source s = case s of
  Patch.MkKept index -> Value.object [Value.pair "kept" . Value.integer $ toInteger index]
  Patch.MkCreated node -> Value.object [Value.pair "node" $ html node]

-- | A node as the page builds it; handlers stay in Haskell, and keys only
-- steer the diff.
html :: Html.Html msg -> Value.Value
html node = case node of
  Html.MkText t -> Value.object [Value.pair "text" $ Value.text t]
  Html.MkElement element ->
    Value.object
      [ Value.pair "tag" . Value.text $ Html.tag element,
        Value.pair "props" . Value.array . foldMap property $ Html.attributes element,
        Value.pair "children" . Value.array . fmap html . Foldable.toList . Html.childList $ Html.children element
      ]
  where
    property attribute = case attribute of
      Attribute.MkProperty name value -> [Value.array [Value.text name, Value.text value]]
      Attribute.MkOn _ _ -> []

-- | An event's path as the page sends it: child indices joined by dots, the
-- empty text for the root. Nothing for anything else.
eventPath :: Text.Text -> Maybe (Seq.Seq Natural)
eventPath t
  | Text.null t = Just Seq.empty
  | otherwise = fmap Seq.fromList . traverse index $ Text.splitOn (Text.pack ".") t
  where
    -- Natural's Read throws on a minus sign rather than failing.
    index i = if not (Text.null i) && Text.all Char.isDigit i then Read.readMaybe $ Text.unpack i else Nothing
