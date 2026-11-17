-- | The patches that bring the DOM from one view to the next. Event handlers
-- never reach the DOM (the page dispatches each event through the current
-- view, Html.handler), so a change to one alone patches nothing.
module Parun.Web.Diff where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch

diff :: Html.Html msg -> Html.Html msg -> Seq.Seq (Patch.Patch msg)
diff = diffAt Seq.empty

diffAt :: Seq.Seq Natural -> Html.Html msg -> Html.Html msg -> Seq.Seq (Patch.Patch msg)
diffAt path old new = case (old, new) of
  (Html.MkText a, Html.MkText b) | a == b -> Seq.empty
  (Html.MkElement o, Html.MkElement n)
    | Html.tag o == Html.tag n,
      Just kids <- children path (Html.children o) (Html.children n) ->
        properties path (Html.attributes o) (Html.attributes n) <> kids
  _ -> Seq.singleton $ Patch.MkReplace path new

-- | Each property by name, the last one given winning.
propertyMap :: Seq.Seq (Attribute.Attribute msg) -> Map.Map Text.Text Text.Text
propertyMap = Foldable.foldl' (\m attribute -> case attribute of Attribute.MkProperty name value -> Map.insert name value m; Attribute.MkOn _ _ -> m) Map.empty

properties :: Seq.Seq Natural -> Seq.Seq (Attribute.Attribute msg) -> Seq.Seq (Attribute.Attribute msg) -> Seq.Seq (Patch.Patch msg)
properties path old new =
  let before = propertyMap old
      after = propertyMap new
      removed = fmap (Patch.MkRemoveProperty path) . Seq.fromList . Map.keys $ Map.difference before after
      set = fmap (uncurry $ Patch.MkSetProperty path) . Seq.fromList . Map.toList $ Map.differenceWith (\b a -> if a == b then Nothing else Just b) after before
   in removed <> set

-- | Nothing where one view keys its children and the other does not, which
-- replaces the element.
children :: Seq.Seq Natural -> Html.Children msg -> Html.Children msg -> Maybe (Seq.Seq (Patch.Patch msg))
children path old new = case (old, new) of
  (Html.MkUnkeyed o, Html.MkUnkeyed n) -> Just $ unkeyed path o n
  (Html.MkKeyed o, Html.MkKeyed n) -> Just $ keyed path o n
  _ -> Nothing

unkeyed :: Seq.Seq Natural -> Seq.Seq (Html.Html msg) -> Seq.Seq (Html.Html msg) -> Seq.Seq (Patch.Patch msg)
unkeyed path old new =
  let common = Seq.zip old new
      changed = Foldable.fold $ Seq.mapWithIndex (\i (o, n) -> diffAt (path Seq.|> Int.toNaturalSaturating i) o n) common
      tidy
        | Seq.length new > Seq.length old = Seq.singleton . Patch.MkAppend path $ Seq.drop (Seq.length old) new
        | Seq.length new < Seq.length old = Seq.singleton . Patch.MkTruncate path $ Natural.length new
        | otherwise = Seq.empty
   in changed <> tidy

-- | Each new child takes the DOM node of the first unclaimed old child with
-- its key; the arrangement comes first, then each kept child's own patches
-- at its new index.
keyed :: Seq.Seq Natural -> Seq.Seq (Text.Text, Html.Html msg) -> Seq.Seq (Text.Text, Html.Html msg) -> Seq.Seq (Patch.Patch msg)
keyed path old new =
  let unclaimed = Map.fromListWith (flip (<>)) . Foldable.toList $ Seq.mapWithIndex (\i (key, html) -> (key, [(Int.toNaturalSaturating i, html)])) old
      claim (pool, sources) (key, html) = case Map.lookup key pool of
        Just ((i, before) : rest) -> (Map.insert key rest pool, sources Seq.|> Left (i, before, html))
        _ -> (pool, sources Seq.|> Right html)
      placed = snd $ Foldable.foldl' claim (unclaimed, Seq.empty) new
      arrangement = fmap (either (\(i, _, _) -> Patch.MkKept i) Patch.MkCreated) placed
      untouched = Seq.length old == Seq.length new && and (Seq.mapWithIndex (\j source -> case source of Patch.MkKept i -> Natural.toInt i == Just j; Patch.MkCreated _ -> False) arrangement)
      arrange = if untouched then Seq.empty else Seq.singleton $ Patch.MkArrange path arrangement
      kept = Foldable.fold $ Seq.mapWithIndex (\j source -> either (\(_, before, after) -> diffAt (path Seq.|> Int.toNaturalSaturating j) before after) (const Seq.empty) source) placed
   in arrange <> kept

-- | The patches applied to a view's tree as the page applies them to the DOM,
-- Nothing where one does not fit; a created keyed child gets the empty key.
-- The specs check diff against it.
apply :: Seq.Seq (Patch.Patch msg) -> Html.Html msg -> Maybe (Html.Html msg)
apply patches html = Foldable.foldlM (flip applyOne) html patches

applyOne :: Patch.Patch msg -> Html.Html msg -> Maybe (Html.Html msg)
applyOne patch = case patch of
  Patch.MkReplace path new -> modifyAt path (const $ Just new)
  Patch.MkSetProperty path name value -> modifyAt path . onNode $ \node -> Just node {Html.attributes = Seq.filter (not . isProperty name) (Html.attributes node) Seq.|> Attribute.MkProperty name value}
  Patch.MkRemoveProperty path name -> modifyAt path . onNode $ \node -> Just node {Html.attributes = Seq.filter (not . isProperty name) (Html.attributes node)}
  Patch.MkAppend path new -> modifyAt path . onNode $ \node -> case Html.children node of
    Html.MkUnkeyed items -> Just node {Html.children = Html.MkUnkeyed (items <> new)}
    Html.MkKeyed _ -> Nothing
  Patch.MkTruncate path n -> modifyAt path . onNode $ \node -> case Html.children node of
    Html.MkUnkeyed items -> Just node {Html.children = Html.MkUnkeyed (Seq.take (Natural.toIntSaturating n) items)}
    Html.MkKeyed _ -> Nothing
  Patch.MkArrange path sources -> modifyAt path . onNode $ \node -> case Html.children node of
    Html.MkKeyed items -> do
      rebuilt <- traverse (\source -> case source of Patch.MkKept i -> Natural.toInt i >>= \n -> Seq.lookup n items; Patch.MkCreated h -> Just (Text.empty, h)) sources
      Just node {Html.children = Html.MkKeyed rebuilt}
    Html.MkUnkeyed _ -> Nothing
  where
    isProperty name attribute = case attribute of
      Attribute.MkProperty other _ -> other == name
      Attribute.MkOn _ _ -> False
    onNode f html = case html of
      Html.MkElement node -> fmap Html.MkElement $ f node
      Html.MkText _ -> Nothing

modifyAt :: Seq.Seq Natural -> (Html.Html msg -> Maybe (Html.Html msg)) -> Html.Html msg -> Maybe (Html.Html msg)
modifyAt path f html = case Seq.viewl path of
  Seq.EmptyL -> f html
  i Seq.:< rest -> case html of
    Html.MkText _ -> Nothing
    Html.MkElement node -> do
      n <- Natural.toInt i
      kids <- case Html.children node of
        Html.MkUnkeyed items -> do
          item <- Seq.lookup n items
          changed <- modifyAt rest f item
          Just . Html.MkUnkeyed $ Seq.update n changed items
        Html.MkKeyed items -> do
          (key, item) <- Seq.lookup n items
          changed <- modifyAt rest f item
          Just . Html.MkKeyed $ Seq.update n (key, changed) items
      Just $ Html.MkElement node {Html.children = kids}
