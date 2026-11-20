-- | The client's own icons, apart from the game's symbols (Parun.Web.Mana):
-- each a @<symbol>@ in the page's sprite (index.html), drawn in the text's
-- colour.
module Parun.Web.Icon where

import qualified Data.Text as Text
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

-- | An icon the sprite draws; Parun.Web.IconSpec proves the two lists equal.
data Icon
  = -- | The beginning phase: a sunrise.
    MkBeginning
  | -- | A main phase: a card.
    MkMain
  | -- | The combat phase: crossed swords.
    MkCombat
  | -- | The ending phase: a sunset.
    MkEnding
  | -- | A hand of cards.
    MkHand
  | -- | A library: a stack of cards.
    MkLibrary
  | -- | A graveyard: a headstone.
    MkGraveyard
  | -- | Exile: a spiral.
    MkExile
  | -- | The settings: a cog.
    MkGear
  | -- | The game's menu: three lines.
    MkMenu
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The id of an icon's @<symbol>@ in the sprite.
iconId :: Icon -> Text.Text
iconId i = Text.pack . ("ui-" <>) $ case i of
  MkBeginning -> "beginning"
  MkMain -> "main"
  MkCombat -> "combat"
  MkEnding -> "ending"
  MkHand -> "hand"
  MkLibrary -> "library"
  MkGraveyard -> "graveyard"
  MkExile -> "exile"
  MkGear -> "gear"
  MkMenu -> "menu"

-- | An icon, hidden from assistive technology: what it means is said by
-- the element holding it.
icon :: [String] -> Icon -> Html.Html msg
icon classes i =
  Element.el
    "svg"
    ("icon" : classes)
    [Element.attr "viewBox" (Text.pack "0 0 24 24"), Element.attr "aria-hidden" (Text.pack "true"), Element.attr "focusable" (Text.pack "false")]
    [Element.el "use" [] [Element.attr "href" (Text.pack "#" <> iconId i)] []]

-- | A spinner, an arc turning on a faint ring: something is loading.
loading :: [String] -> Html.Html msg
loading classes =
  Element.el
    "svg"
    ("icon" : "spinner" : classes)
    [Element.attr "viewBox" (Text.pack "0 0 24 24"), Element.attr "aria-hidden" (Text.pack "true"), Element.attr "focusable" (Text.pack "false")]
    [ Element.el "circle" ["track"] [Element.attr "cx" (Text.pack "12"), Element.attr "cy" (Text.pack "12"), Element.attr "r" (Text.pack "9")] [],
      Element.el "path" ["arc"] [Element.attr "d" (Text.pack "M12 3a9 9 0 0 1 9 9")] []
    ]
