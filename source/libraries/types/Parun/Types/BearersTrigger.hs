module Parun.Types.BearersTrigger where

import qualified Parun.Types.EntryTriggering as EntryTriggering
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordDesignator as KeywordDesignator

-- | A rule over the triggering of an object's triggered abilities, a
-- permanent's or a dungeon's (Hama Pashar, Ruin Seeker), whatever their event,
-- read for the player whose abilities they are
-- (Parun.Types.PlayerEffect.BearersTrigger).
data BearersTrigger = MkBearersTrigger
  { -- | CR 113.7: whose abilities it applies to, CR 109.5's "you" being the
    -- bearer's controller.
    bearers :: Filter.Filter Keyword.Keyword,
    -- | CR 702: which of them, by the keyword that minted them (Nowhere to
    -- Run's "ward abilities"), or Nothing for every one (Sanctum of All).
    abilities :: Maybe (KeywordDesignator.KeywordDesignator Keyword.Keyword),
    triggering :: EntryTriggering.EntryTriggering
  }
  deriving (Eq, Ord, Show)
