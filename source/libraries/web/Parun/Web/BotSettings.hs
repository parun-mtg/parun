-- | How a bot plays, as the page chooses it: its difficulty and its
-- persona (Parun.Play.Opponent), shared by the lobby (Parun.Web.Lobby) and
-- a replay played on (Parun.Web.Replay).
module Parun.Web.BotSettings where

import qualified Data.Text as Text
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Weights as Weights
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

-- | How a bot plays unless told otherwise: as `parun play`'s does.
defaultBot :: (Difficulty.Difficulty, Persona.Persona)
defaultBot = (Difficulty.MkHard, Persona.MkBalanced)

-- | How the search bot plays at this difficulty and persona: its budget and
-- weights (Parun.Play.Opponent).
botPlay :: (Difficulty.Difficulty, Persona.Persona) -> (SearchBudget.SearchBudget, Weights.Weights)
botPlay (level, style) = (Opponent.budgetAt level PlayOptions.budget, Opponent.weightsOf style)

-- | How a bot plays: its difficulty and its persona, each a choice of those
-- named (Parun.Play.Opponent) sending `pickLevel` or `pickStyle`, the
-- menus' ids ending in `suffix`.
botView :: String -> e -> e -> (Difficulty.Difficulty, Persona.Persona) -> Html.Html e
botView suffix pickLevel pickStyle (level, style) =
  Element.el
    "div"
    ["bot-settings"]
    []
    [ Element.el "label" [] [Element.attr "for" (Text.pack levelId)] [Element.string "Level"],
      choice levelId "bot-difficulty" pickLevel Opponent.difficultyName level,
      Element.el "label" [] [Element.attr "for" (Text.pack styleId)] [Element.string "Style"],
      choice styleId "bot-persona" pickStyle Opponent.personaName style
    ]
  where
    levelId = "bot-difficulty-" <> suffix
    styleId = "bot-persona-" <> suffix
    choice :: (Bounded a, Enum a, Eq a) => String -> String -> e -> (a -> (String, String)) -> a -> Html.Html e
    choice field kind event nameOf current =
      Element.el
        "select"
        [kind, "deck-menu"]
        [Element.attr "id" (Text.pack field), Element.on "change" event]
        [ Element.el "option" [] ([Element.attr "value" (Text.pack value)] <> [Element.attr "selected" Text.empty | option == current]) [Element.string label]
        | option <- [minBound .. maxBound],
          let (value, label) = nameOf option
        ]
