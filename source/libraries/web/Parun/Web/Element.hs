-- | Shorthand for building views: an element by tag and class names, text,
-- and the attributes the client's pages use. Tags, classes and attribute
-- names are literals of the view's own, so they are given as strings.
module Parun.Web.Element where

import qualified Data.Text as Text
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html

-- | An element with these classes (none for the empty list) and attributes.
el :: String -> [String] -> [Attribute.Attribute msg] -> [Html.Html msg] -> Html.Html msg
el tag classes attrs = Html.element (Text.pack tag) (classed classes <> attrs)

-- | An element whose children are matched by key (Html.keyed).
keyed :: String -> [String] -> [Attribute.Attribute msg] -> [(Text.Text, Html.Html msg)] -> Html.Html msg
keyed tag classes attrs = Html.keyed (Text.pack tag) (classed classes <> attrs)

classed :: [String] -> [Attribute.Attribute msg]
classed classes = case filter (not . null) classes of
  [] -> []
  names -> [attr "class" . Text.pack $ unwords names]

attr :: String -> Text.Text -> Attribute.Attribute msg
attr name = Attribute.MkProperty (Text.pack name)

on :: String -> msg -> Attribute.Attribute msg
on event = Attribute.MkOn (Text.pack event)

text :: Text.Text -> Html.Html msg
text = Html.text

string :: String -> Html.Html msg
string = Html.text . Text.pack

-- | A button sending the message when tapped, with these classes.
button :: [String] -> msg -> [Html.Html msg] -> Html.Html msg
button classes msg = el "button" classes [attr "type" (Text.pack "button"), on "click" msg]
