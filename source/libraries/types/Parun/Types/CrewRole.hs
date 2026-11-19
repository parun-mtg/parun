module Parun.Types.CrewRole where

import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.TriggerFrequency as TriggerFrequency

-- | CR 702.122 / 702.171c / 702.154c: which side of a tap-to-pay relation the
-- bearer of a trigger is.
data CrewRole
  = -- | CR 702.122e: the Vehicle, crewed as its crew ability resolves (Mobilizer
    -- Mech); the frequency is Mighty Servant of Leuk-o's "for the first time
    -- each turn".
    Vehicle TriggerFrequency.TriggerFrequency
  | -- | CR 702.122b / 702.171c: a creature tapped to pay the named keyword's
    -- cost, crew (Gearshift Ace) or saddle (Canyon Vaulter).
    Crewer KeywordFamily.KeywordFamily
  | -- | CR 702.154c: the bearer enlisted a creature (Guardian of New Benalia).
    Enlister
  deriving (Eq, Ord, Show)
