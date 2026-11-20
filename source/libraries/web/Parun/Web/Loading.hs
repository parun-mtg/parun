-- | What the web client has loaded of its data, chunk by chunk (engine.js),
-- and what its screens wait on.
module Parun.Web.Loading where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

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
problem loading =
  let said need = case need of
        MkDecks -> "decks"
        MkCards -> "cards"
        MkRules -> "rules"
        MkScenarios -> "scenarios"
   in List.find (\n -> Maybe.isJust (blocked n loading)) [minBound .. maxBound] >>= \n -> Just (Text.pack ("Couldn't load the " <> said n <> ". Reload to try again."))
