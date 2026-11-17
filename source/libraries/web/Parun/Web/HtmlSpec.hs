module Parun.Web.HtmlSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Web.Html" $ do
  Spec.describe s "toText" $ do
    Spec.it s "renders an element, its properties and children" $ do
      Spec.assertEq s (Html.toText page) $ Text.pack "<ul class=\"zone\"><li>a</li><li id=\"b\">b</li></ul>"

    Spec.it s "escapes text and property values" $ do
      Spec.assertEq s (Html.toText (Html.element (Text.pack "p") [Attribute.MkProperty (Text.pack "title") (Text.pack "\"")] [Html.text $ Text.pack "<&>"] :: Html.Html ())) $ Text.pack "<p title=\"&quot;\">&lt;&amp;&gt;</p>"

  Spec.describe s "handler" $ do
    Spec.it s "finds the handler of the node at the path" $ do
      Spec.assertEq s (Html.handler click (Seq.fromList [1]) page) $ Just 'b'

    Spec.it s "bubbles to the nearest ancestor with one" $ do
      Spec.assertEq s (Html.handler click (Seq.fromList [0]) page) $ Just 'u'

    Spec.it s "bubbles from a text node" $ do
      Spec.assertEq s (Html.handler click (Seq.fromList [1, 0]) page) $ Just 'b'

    Spec.it s "ignores another event's handler" $ do
      Spec.assertEq s (Html.handler (Text.pack "input") (Seq.fromList [1]) page) Nothing

    Spec.it s "finds nothing at a path leading nowhere" $ do
      Spec.assertEq s (Html.handler click (Seq.fromList [2]) page) Nothing

click :: Text.Text
click = Text.pack "click"

page :: Html.Html Char
page =
  Html.element
    (Text.pack "ul")
    [Attribute.MkProperty (Text.pack "class") (Text.pack "zone"), Attribute.MkOn click 'u']
    [ Html.element (Text.pack "li") [] [Html.text $ Text.pack "a"],
      Html.element (Text.pack "li") [Attribute.MkProperty (Text.pack "id") (Text.pack "b"), Attribute.MkOn click 'b'] [Html.text $ Text.pack "b"]
    ]
