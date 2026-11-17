module Parun.Web.DiffSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Diff as Diff
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Patch as Patch

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Diff" $ do
  Spec.describe s "diff" $ do
    Spec.it s "patches nothing between equal views" $ do
      Spec.assertEq s (Diff.diff (zones "ab" "c") (zones "ab" "c")) Seq.empty

    Spec.it s "patches nothing for a changed handler" $ do
      Spec.assertEq s (Diff.diff (button 'a') (button 'b')) Seq.empty

    Spec.it s "replaces a changed text node" $ do
      Spec.assertEq s (Diff.diff (li "a") (li "b")) . Seq.singleton . Patch.MkReplace (Seq.singleton 0) . Html.text $ Text.pack "b"

    Spec.it s "replaces an element whose tag changed" $ do
      Spec.assertEq s (Diff.diff (li "a") (Html.element (Text.pack "p") [] [])) . Seq.singleton . Patch.MkReplace Seq.empty $ Html.element (Text.pack "p") [] []

    Spec.it s "sets a changed property and removes a dropped one" $ do
      Spec.assertEq s (Diff.diff (Html.element divTag [cls "x", ident "y"] []) (Html.element divTag [cls "z"] [] :: Html.Html ())) $
        Seq.fromList [Patch.MkRemoveProperty Seq.empty (Text.pack "id"), Patch.MkSetProperty Seq.empty (Text.pack "class") (Text.pack "z")]

    Spec.it s "appends children" $ do
      Spec.assertEq s (Diff.diff (list "a") (list "abc")) . Seq.singleton . Patch.MkAppend Seq.empty $ Seq.fromList [li "b", li "c"]

    Spec.it s "truncates children" $ do
      Spec.assertEq s (Diff.diff (list "abc") (list "a")) . Seq.singleton $ Patch.MkTruncate Seq.empty 1

    Spec.it s "keeps a keyed child's node when it moves between zones" $ do
      Spec.assertEq s (Diff.diff (zones "ab" "c") (zones "b" "ca")) $
        Seq.fromList [Patch.MkArrange (Seq.singleton 0) (Seq.singleton (Patch.MkKept 1)), Patch.MkArrange (Seq.singleton 1) (Seq.fromList [Patch.MkKept 0, Patch.MkCreated (card 'a')])]

    Spec.it s "moves keyed children rather than replacing them" $ do
      Spec.assertEq s (Diff.diff (zone "abc") (zone "cab")) . Seq.singleton . Patch.MkArrange Seq.empty $ Seq.fromList [Patch.MkKept 2, Patch.MkKept 0, Patch.MkKept 1]

    Spec.it s "patches a moved keyed child at its new index" $ do
      Spec.assertEq s (Diff.diff (zone "ab") (Html.keyed ulTag [] [(key 'b', Html.element liTag [cls "tapped"] [Html.text $ Text.pack "b"]), (key 'a', card 'a')])) $
        Seq.fromList [Patch.MkArrange Seq.empty (Seq.fromList [Patch.MkKept 1, Patch.MkKept 0]), Patch.MkSetProperty (Seq.singleton 0) (Text.pack "class") (Text.pack "tapped")]

    Spec.it s "replaces an element that starts keying its children" $ do
      Spec.assertEq s (Diff.diff (list "a") (zone "a")) . Seq.singleton $ Patch.MkReplace Seq.empty (zone "a")

    Spec.it s "brings every old view to the new one" $ do
      Foldable.for_ cases $ \(old, new) ->
        Spec.assertEqWith s (show (Html.toText old, Html.toText new)) (fmap shape (Diff.apply (Diff.diff old new) old)) . Just $ shape new

-- | A view as the DOM sees it: no handlers, no keys, properties by name.
shape :: Html.Html msg -> Html.Html ()
shape html = case html of
  Html.MkText t -> Html.MkText t
  Html.MkElement node ->
    Html.MkElement $
      Html.MkNode
        (Html.tag node)
        (fmap (uncurry Attribute.MkProperty) . Seq.fromList . Map.toList $ Diff.propertyMap (Html.attributes node))
        (Html.MkUnkeyed . fmap shape . Html.childList $ Html.children node)

cases :: [(Html.Html (), Html.Html ())]
cases =
  let views = [zones "" "", zones "ab" "c", zones "b" "ca", zones "cba" "", zones "aab" "b", list "", list "ab", list "ba", zone "abc", button (), Html.text $ Text.pack "t"]
   in concatMap (\old -> fmap (\new -> (old, new)) views) views

divTag, ulTag, liTag :: Text.Text
divTag = Text.pack "div"
ulTag = Text.pack "ul"
liTag = Text.pack "li"

cls, ident :: String -> Attribute.Attribute msg
cls = Attribute.MkProperty (Text.pack "class") . Text.pack
ident = Attribute.MkProperty (Text.pack "id") . Text.pack

key :: Char -> Text.Text
key = Text.singleton

li :: String -> Html.Html ()
li t = Html.element liTag [] [Html.text $ Text.pack t]

card :: Char -> Html.Html ()
card c = li [c]

list :: String -> Html.Html ()
list = Html.element ulTag [] . fmap card

zone :: String -> Html.Html ()
zone = Html.keyed ulTag [] . fmap (\c -> (key c, card c))

zones :: String -> String -> Html.Html ()
zones hand battlefield = Html.element divTag [] [zone hand, zone battlefield]

button :: msg -> Html.Html msg
button msg = Html.element (Text.pack "button") [Attribute.MkOn (Text.pack "click") msg] [Html.text $ Text.pack "go"]
