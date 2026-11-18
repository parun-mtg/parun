-- | Text with its brace-delimited symbols ({G}, {2}, {T}, {W/U}, CR 107)
-- drawn as symbols: each a span the style sheet colours by what it names,
-- with the symbol's own text kept for anyone reading the page as text.
module Parun.Web.Mana where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Parun.Play.Render as Render
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Text.Read as Read

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
-- or by the symbol's own letter otherwise; drawn in the Mana font where it
-- has a glyph for every part (style.css), else as its letters; and read
-- aloud as words.
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
      -- A Phyrexian symbol is drawn as the Phyrexian glyph in its colour.
      drawn = if length parts == 2 && Text.pack "P" `elem` parts then [Text.pack "P"] else parts
      glyphs = traverse glyph drawn
      letters = case Text.unpack (Text.toUpper inside) of
        "T" -> Text.pack "\x21b7"
        "Q" -> Text.pack "\x21b6"
        _ -> Text.filter (/= '/') (Text.replace (Text.pack "/P") Text.empty (Text.toUpper inside))
      shape = if length parts > 1 then ["hybrid"] else []
   in Element.el
        "span"
        (["ms"] <> shape <> fmap ("ms-" <>) kinds <> foldMap (const ["glyph"]) glyphs)
        [Element.attr "title" (Text.pack "{" <> inside <> Text.pack "}"), Element.attr "role" (Text.pack "img"), Element.attr "aria-label" (spoken parts)]
        [Element.text (maybe letters Text.concat glyphs)]

-- | A symbol part's glyph in the Mana font (its private-use code points).
glyph :: Text.Text -> Maybe Text.Text
glyph part = fmap Text.singleton $ case Text.unpack part of
  "W" -> Just '\xe600'
  "U" -> Just '\xe601'
  "B" -> Just '\xe602'
  "R" -> Just '\xe603'
  "G" -> Just '\xe604'
  "C" -> Just '\xe904'
  "S" -> Just '\xe619'
  "X" -> Just '\xe615'
  "Y" -> Just '\xe616'
  "Z" -> Just '\xe617'
  "P" -> Just '\xe618'
  "T" -> Just '\xe61a'
  "Q" -> Just '\xe61b'
  "E" -> Just '\xe907'
  digits -> do
    n <- Read.readMaybe digits :: Maybe Int
    Monad.guard (n >= 0 && n <= 20)
    -- 0 to 15 run on from U+E605; 16 to 20 from U+E62A.
    Just (Char.chr (if n <= 15 then 0xe605 + n else 0xe62a + n - 16))

-- | A symbol read aloud: "green", "2 generic", "white or blue",
-- "Phyrexian green", "tap".
spoken :: [Text.Text] -> Text.Text
spoken parts =
  let word part = case Text.unpack part of
        "W" -> "white"
        "U" -> "blue"
        "B" -> "black"
        "R" -> "red"
        "G" -> "green"
        "C" -> "colorless"
        "S" -> "snow"
        "T" -> "tap"
        "Q" -> "untap"
        "E" -> "energy"
        "X" -> "X"
        "Y" -> "Y"
        "Z" -> "Z"
        other -> other <> " generic"
   in case parts of
        [colour, p] | p == Text.pack "P" -> Text.pack ("Phyrexian " <> word colour)
        _ -> Text.intercalate (Text.pack " or ") (fmap (Text.pack . word) parts)
