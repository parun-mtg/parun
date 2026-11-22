-- | What the web client has loaded of its data, chunk by chunk (engine.js),
-- what its screens wait on, and the screen shown while they wait.
module Parun.Web.Loading where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Int as Int
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

-- | Something a screen waits on.
data Need
  = -- | The deck lists and formats.
    MkDecks
  | -- | Every card.
    MkCards
  | -- | The legalities, printings and sticker sheets a game needs.
    MkRules
  | -- | The scenarios a replay plays.
    MkScenarios
  deriving (Eq, Ord, Show, Enum, Bounded)

data Loading = MkLoading
  { -- | The chunks merged into the data directory.
    arrived :: Set.Set Text.Text,
    -- | The chunks that failed to download.
    failed :: Set.Set Text.Text,
    -- | What screens have waited on, first first.
    wanted :: [Need]
  }
  deriving (Eq, Show)

-- | Nothing loaded, nothing wanted.
new :: Loading
new = MkLoading Set.empty Set.empty []

-- | The chunks a need is made of (script/build-web.sh).
chunksOf :: Need -> [Text.Text]
chunksOf need = fmap Text.pack $ case need of
  MkDecks -> ["decks"]
  MkCards -> ["cards-" <> show k | k <- [0 :: Int .. 3]]
  MkRules -> ["rules"]
  MkScenarios -> ["scenarios"]

-- | A chunk merged (Right) or failed (Left).
arrive :: Either Text.Text Text.Text -> Loading -> Loading
arrive chunk loading = case chunk of
  Right name -> loading {arrived = Set.insert name (arrived loading)}
  Left name -> loading {failed = Set.insert name (failed loading)}

-- | Whether every chunk of the need has arrived.
ready :: Need -> Loading -> Bool
ready need loading = all (`Set.member` arrived loading) (chunksOf need)

-- | The first chunk of the need that failed, if any.
blocked :: Need -> Loading -> Maybe Text.Text
blocked need loading = List.find (`Set.member` failed loading) (chunksOf need)

-- | The need recorded as wanted, and its chunks yet to arrive, to ask the
-- worker for.
demand :: Need -> Loading -> (Loading, [Text.Text])
demand need loading =
  ( loading {wanted = if need `elem` wanted loading then wanted loading else wanted loading <> [need]},
    filter (`Set.notMember` arrived loading) (chunksOf need)
  )

-- | Why the first need that cannot load did not, said for the screen
-- waiting on it.
problem :: Loading -> Maybe Text.Text
problem loading = Maybe.listToMaybe (Maybe.mapMaybe (`problemFor` loading) [minBound .. maxBound])

-- | Why this need did not load, if it cannot.
problemFor :: Need -> Loading -> Maybe Text.Text
problemFor need loading =
  let said = case need of
        MkDecks -> "decks"
        MkCards -> "cards"
        MkRules -> "rules"
        MkScenarios -> "scenarios"
   in Text.pack ("Couldn't load the " <> said <> ". Reload to try again.") <$ blocked need loading

-- | Where a step of loading stands.
data Step
  = MkDone
  | -- | Under way, this percentage through where that is known.
    MkDoing (Maybe Natural)
  | MkWaiting
  deriving (Eq, Show)

-- | The steps a screen waits through once the engine runs, as the splash
-- (index.html) names them: the engine, started; the cards, as far as their
-- chunks have arrived; then the rest of what the screen waits on, read and
-- built, how far unknown.
steps :: Loading -> [(String, Step)]
steps loading =
  let cards = chunksOf MkCards
      fetched = length (filter (`Set.member` arrived loading) cards)
   in [ ("Starting the engine", MkDone),
        ("Fetching the cards", if ready MkCards loading then MkDone else MkDoing (Just (Int.toNaturalSaturating (div (100 * fetched) (length cards))))),
        ("Setting the table", if ready MkCards loading then MkDoing Nothing else MkWaiting)
      ]

-- | The screen shown while loading: the logo in a turning ring, and the
-- steps with where each stands.
view :: Loading -> Html.Html msg
view loading = Element.el "main" ["message", "loading"] [Element.attr "aria-busy" (Text.pack "true")] [ring, stepList (steps loading)]

-- | The logo in a ring whose arc turns while something loads (index.html's
-- splash draws the same).
ring :: Html.Html msg
ring =
  Element.el
    "div"
    ["ring"]
    []
    [ Element.el
        "svg"
        []
        [Element.attr "viewBox" (Text.pack "0 0 168 168"), Element.attr "aria-hidden" (Text.pack "true"), Element.attr "focusable" (Text.pack "false")]
        [ Element.el "circle" ["track"] [Element.attr "cx" (Text.pack "84"), Element.attr "cy" (Text.pack "84"), Element.attr "r" (Text.pack "78")] [],
          Element.el "circle" ["arc"] [Element.attr "cx" (Text.pack "84"), Element.attr "cy" (Text.pack "84"), Element.attr "r" (Text.pack "78")] []
        ],
      Element.el "img" ["logo"] [Element.attr "src" (Text.pack "./icon.svg"), Element.attr "alt" Text.empty, Element.attr "width" (Text.pack "104"), Element.attr "height" (Text.pack "104")] []
    ]

-- | Each step, marked done, under way or waiting, said so beside it: done,
-- or the percentage where one is known.
stepList :: [(String, Step)] -> Html.Html msg
stepList listed =
  Element.el
    "ol"
    ["steps", "panel"]
    [Element.attr "aria-label" (Text.pack "Loading progress")]
    [ Element.el
        "li"
        [stateClass step]
        []
        [Element.el "span" ["dot"] [Element.attr "aria-hidden" (Text.pack "true")] [], Element.el "span" ["label"] [] [Element.string label], Element.el "span" ["state"] [] [Element.string (said step)]]
    | (label, step) <- listed
    ]
  where
    stateClass step = case step of
      MkDone -> "done"
      MkDoing _ -> "now"
      MkWaiting -> "waiting"
    said step = case step of
      MkDone -> "Done"
      MkDoing percent -> foldMap (\p -> show p <> "%") percent
      MkWaiting -> ""
