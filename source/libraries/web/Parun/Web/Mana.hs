-- | Text with its brace-delimited symbols ({G}, {2}, {T}, {W/U}, CR 107)
-- drawn as symbols: each a span the style sheet colours by what it names,
-- with the symbol's own text kept for anyone reading the page as text.
module Parun.Web.Mana where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Parun.Play.Render as Render
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

-- | The text, each symbol in it drawn; text outside braces as it is.
rich :: Text.Text -> [Html.Html msg]
rich t =
  let (before, rest) = Text.breakOn (Text.pack "{") t
      plain = [Element.text before | not (Text.null before)]
   in case Text.uncons rest of
        Nothing -> plain
        Just (_, afterBrace) ->
          let (inside, closing) = Text.breakOn (Text.pack "}") afterBrace
           in if Text.null closing || Text.null inside || Text.any (\c -> Char.isSpace c || c == '{') inside
                then plain <> [Element.text (Text.pack "{")] <> rich afterBrace
                else plain <> [symbol inside] <> rich (Text.drop 1 closing)

-- | A cost's symbols, drawn.
cost :: ManaCost.ManaCost -> Html.Html msg
cost c = Element.el "span" ["cost"] [] . rich $ Render.manaCost c

-- | One symbol, by what is inside its braces: a span classed for each mana
-- type it names (two for a hybrid), "generic" for a number or a variable,
-- or by the symbol's own letter otherwise.
symbol :: Text.Text -> Html.Html msg
symbol inside =
  let parts = Text.splitOn (Text.pack "/") (Text.toUpper inside)
      kinds = concatMap kind parts
      kind part = case Text.unpack part of
        "W" -> ["w"]
        "U" -> ["u"]
        "B" -> ["b"]
        "R" -> ["r"]
        "G" -> ["g"]
        "C" -> ["c"]
        "S" -> ["snow"]
        "T" -> ["tap"]
        "Q" -> ["untap"]
        "E" -> ["energy"]
        "P" -> ["phyrexian"]
        "X" -> ["generic"]
        "Y" -> ["generic"]
        "Z" -> ["generic"]
        s | not (null s), all Char.isDigit s -> ["generic"]
        _ -> ["other"]
      glyph = case Text.unpack (Text.toUpper inside) of
        "T" -> Text.pack "\x21b7"
        "Q" -> Text.pack "\x21b6"
        _ -> Text.filter (/= '/') (Text.replace (Text.pack "/P") Text.empty (Text.toUpper inside))
      shape = if length parts > 1 then ["hybrid"] else []
   in Element.el "span" (["ms"] <> shape <> fmap ("ms-" <>) kinds) [Element.attr "title" (Text.pack "{" <> inside <> Text.pack "}")] [Element.text glyph]
