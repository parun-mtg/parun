-- | Text with its symbols drawn: the brace-delimited ones ({G}, {2}, {T},
-- {W/U}, CR 107) and loyalty costs ([+1], CR 107.7), each in Parun's own
-- drawings, its own text kept as its tooltip and read aloud as words.
module Parun.Web.Mana where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Numeric
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

-- | The text, each symbol in it drawn; text outside them as it is.
rich :: Text.Text -> [Html.Html msg]
rich t =
  let (before, rest) = Text.break (\c -> c == '{' || c == '[') t
      plain = [Element.text before | not (Text.null before)]
   in case Text.uncons rest of
        Nothing -> plain
        Just (opening, afterOpening) ->
          let (inside, closing) = Text.breakOn (Text.singleton (if opening == '{' then '}' else ']')) afterOpening
              drawn = if opening == '{' then braced inside else loyalty inside
           in case drawn of
                Just html | not (Text.null closing) -> plain <> [html] <> rich (Text.drop 1 closing)
                _ -> plain <> [Element.text (Text.singleton opening)] <> rich afterOpening
  where
    braced inside = if Text.null inside || Text.any (\c -> Char.isSpace c || c == '{') inside then Nothing else Just (symbol inside)

-- | A cost's symbols, drawn.
cost :: ManaCost.ManaCost -> Html.Html msg
cost c = Element.el "span" ["cost"] [] . rich $ Wording.manaCost c

-- | A motif the page's sprite draws (index.html), as the @<symbol>@ whose
-- id is 'motifId'; Parun.Web.ManaSpec proves the two lists equal.
data Motif
  = MkWhite
  | MkBlue
  | MkBlack
  | MkRed
  | MkGreen
  | MkColorless
  | MkSnow
  | MkTap
  | MkUntap
  | MkEnergy
  | MkPhyrexian
  | MkTicket
  | MkPawprint
  | MkAcorn
  | MkLandDrop
  | MkLegendary
  | MkChaos
  | MkPlaneswalker
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The id of a motif's @<symbol>@ in the sprite.
motifId :: Motif -> Text.Text
motifId motif = Text.pack . ("ms-" <>) $ case motif of
  MkWhite -> "w"
  MkBlue -> "u"
  MkBlack -> "b"
  MkRed -> "r"
  MkGreen -> "g"
  MkColorless -> "c"
  MkSnow -> "snow"
  MkTap -> "tap"
  MkUntap -> "untap"
  MkEnergy -> "energy"
  MkPhyrexian -> "phyrexian"
  MkTicket -> "ticket"
  MkPawprint -> "paw"
  MkAcorn -> "acorn"
  MkLandDrop -> "land-drop"
  MkLegendary -> "legendary"
  MkChaos -> "chaos"
  MkPlaneswalker -> "planeswalker"

-- | What a circle, or one half of it, shows.
data Mark
  = -- | A motif from the sprite.
    MkMotif Motif
  | -- | Text: {2/W}'s 2.
    MkLetters Text.Text
  deriving (Eq, Show)

-- | A tint, the style sheet's @bg-@ and @fg-@ classes, and a mark on it.
data Part = MkPart Text.Text Mark
  deriving (Eq, Show)

-- | A symbol's shape.
data Drawing
  = -- | Text in a pill: a number, a variable, or a symbol with no motif.
    MkPill Text.Text
  | -- | One circle.
    MkWhole Part
  | -- | A circle split corner to corner, the first part above (CR 107.4e).
    MkSplit Part Part
  | -- | The left half of a circle ({HW}).
    MkHalf Part
  deriving (Eq, Show)

-- | A colour letter's part.
coloured :: Text.Text -> Maybe Part
coloured letter = case Text.unpack letter of
  "W" -> Just $ tinted "w" MkWhite
  "U" -> Just $ tinted "u" MkBlue
  "B" -> Just $ tinted "b" MkBlack
  "R" -> Just $ tinted "r" MkRed
  "G" -> Just $ tinted "g" MkGreen
  "C" -> Just $ tinted "c" MkColorless
  _ -> Nothing
  where
    tinted tint = MkPart (Text.pack tint) . MkMotif

-- | The motif of a symbol drawn alone on a plain circle.
standalone :: Text.Text -> Maybe Motif
standalone letters = case Text.unpack letters of
  "S" -> Just MkSnow
  "T" -> Just MkTap
  "Q" -> Just MkUntap
  "E" -> Just MkEnergy
  -- CR 107.4g
  "H" -> Just MkPhyrexian
  -- CR 107.17
  "TK" -> Just MkTicket
  -- CR 107.18: alone, P is the pawprint; after a slash, Phyrexian (CR 107.4f).
  "P" -> Just MkPawprint
  "A" -> Just MkAcorn
  "D" -> Just MkLandDrop
  "L" -> Just MkLegendary
  -- CR 107.12
  "CHAOS" -> Just MkChaos
  -- CR 107.11
  "PW" -> Just MkPlaneswalker
  _ -> Nothing

-- | How the symbol inside these braces is drawn.
drawing :: Text.Text -> Drawing
drawing inside =
  let upper = Text.toUpper inside
      p = Text.pack "P"
      plain = MkPart (Text.pack "generic")
      -- CR 107.4: a hybrid's half is a colour or, in a monocolored hybrid, a number.
      half letters = case coloured letters of
        Nothing | not (Text.null letters), Text.all Char.isDigit letters -> Just (plain (MkLetters letters))
        part -> part
      phyrexian letters = fmap (\(MkPart tint _) -> MkPart tint (MkMotif MkPhyrexian)) (coloured letters)
   in case Text.splitOn (Text.pack "/") upper of
        [one]
          | Just part <- coloured one -> MkWhole part
          | Just motif <- standalone one -> MkWhole (plain (MkMotif motif))
          | Just ('H', colour) <- Text.uncons one, Just part <- coloured colour -> MkHalf part
        [one, two]
          | two == p, Just part <- phyrexian one -> MkWhole part
          | Just first <- half one, Just second <- half two -> MkSplit first second
        [one, two, three]
          | three == p, Just first <- phyrexian one, Just second <- phyrexian two -> MkSplit first second
        _ -> MkPill (Text.filter (/= '/') upper)

-- | One symbol, by what is inside its braces.
symbol :: Text.Text -> Html.Html msg
symbol inside =
  let labelled classes = Element.el "span" ("ms" : classes) [Element.attr "title" (Text.pack "{" <> inside <> Text.pack "}"), Element.attr "role" (Text.pack "img"), Element.attr "aria-label" (spoken inside)]
   in case drawing inside of
        MkPill letters -> labelled ["ms-generic"] [Element.text letters]
        MkWhole part -> labelled [] [svg (whole part)]
        MkSplit first second -> labelled ["hybrid"] [svg [backing first upperLeft, backing second lowerRight, shown first 1.5 18, shown second 12.5 18, rim]]
        MkHalf part -> labelled ["half"] [svg [Element.el "g" [] [Element.attr "clip-path" (Text.pack "url(#ms-left)")] (whole part)]]
  where
    svg = Element.el "svg" [] [Element.attr "viewBox" (Text.pack "0 0 32 32"), Element.attr "aria-hidden" (Text.pack "true")]
    whole part@(MkPart tint _) = [circle [classOf "bg-" tint] 16, shown part 0 32, rim]
    circle classes r = Element.el "circle" classes [number "cx" 16, number "cy" 16, number "r" r] []
    -- An edge, so a pale circle keeps its shape on a pale page.
    rim = circle ["rim"] 15.5
    backing (MkPart tint _) d = Element.el "path" [classOf "bg-" tint] [Element.attr "d" (Text.pack d)] []
    upperLeft = "M27.31 4.69A16 16 0 0 0 4.69 27.31Z"
    lowerRight = "M27.31 4.69A16 16 0 0 1 4.69 27.31Z"
    -- A mark in the square from (at, at), size across.
    shown (MkPart tint mark) at size = case mark of
      MkMotif motif -> Element.el "use" [classOf "fg-" tint] [Element.attr "href" (Text.pack "#" <> motifId motif), number "x" at, number "y" at, number "width" size, number "height" size] []
      MkLetters letters -> Element.el "text" [classOf "fg-" tint] [number "x" (at + size / 2), number "y" (at + size / 2), number "font-size" (size * 0.6)] [Element.text letters]
    classOf prefix tint = prefix <> Text.unpack tint

-- | A coordinate attribute, to two decimal places at most.
number :: String -> Double -> Attribute.Attribute msg
number name n =
  let shown = Numeric.showFFloat (Just 2) n ""
      trimmed = reverse . dropWhile (== '.') . dropWhile (== '0') $ reverse shown
   in Element.attr name (Text.pack trimmed)

-- | A loyalty cost's symbol, by what is inside its brackets: up for a gain,
-- down for a loss, level for zero (CR 107.7); Nothing for anything else
-- in brackets.
loyalty :: Text.Text -> Maybe (Html.Html msg)
loyalty inside = do
  (direction, sign, said, amount) <- case Text.uncons inside of
    Just ('+', rest) -> Just ("up", "+", "plus ", rest)
    Just (c, rest) | c == '-' || c == '\x2212' -> Just ("down", "\x2212", "minus ", rest)
    _ -> Just ("level", "", "", inside)
  let number' = not (Text.null amount) && (Text.all Char.isDigit amount || amount == Text.pack "X")
  if number' && (direction /= "level" || amount == Text.pack "0")
    then Just $ Element.el "span" ["loyalty-cost", direction] [Element.attr "title" (Text.pack "[" <> inside <> Text.pack "]"), Element.attr "role" (Text.pack "img"), Element.attr "aria-label" (Text.pack ("loyalty " <> said) <> amount)] [Element.text (Text.pack sign <> amount)]
    else Nothing

-- | A symbol read aloud: "green", "2 generic", "white or blue",
-- "Phyrexian green", "tap".
spoken :: Text.Text -> Text.Text
spoken inside =
  let word part = case Text.unpack part of
        "W" -> Text.pack "white"
        "U" -> Text.pack "blue"
        "B" -> Text.pack "black"
        "R" -> Text.pack "red"
        "G" -> Text.pack "green"
        "C" -> Text.pack "colorless"
        "S" -> Text.pack "snow"
        "T" -> Text.pack "tap"
        "Q" -> Text.pack "untap"
        "E" -> Text.pack "energy"
        "H" -> Text.pack "Phyrexian"
        "TK" -> Text.pack "ticket"
        "P" -> Text.pack "pawprint"
        "A" -> Text.pack "acorn"
        "D" -> Text.pack "land drop"
        "L" -> Text.pack "legendary"
        "CHAOS" -> Text.pack "chaos"
        "PW" -> Text.pack "planeswalker"
        "X" -> Text.pack "X"
        "Y" -> Text.pack "Y"
        "Z" -> Text.pack "Z"
        'H' : colour | Just _ <- coloured (Text.pack colour) -> Text.pack "half " <> word (Text.pack colour)
        _ -> part <> Text.pack " generic"
      choice = Text.intercalate (Text.pack " or ") . fmap word
   in case reverse (Text.splitOn (Text.pack "/") (Text.toUpper inside)) of
        p : colours@(_ : _) | p == Text.pack "P" -> Text.pack "Phyrexian " <> choice (reverse colours)
        parts -> choice (reverse parts)
