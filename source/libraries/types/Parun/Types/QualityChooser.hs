module Parun.Types.QualityChooser where

import qualified Parun.Types.ChoiceDistinct as ChoiceDistinct

-- | Who makes a Parun.Types.ChooseQuality's choice.
data QualityChooser
  = -- | CR 608.2d: the resolving controller (Persecute, Mystic Compass).
    Controller
  | -- | CR 101.4: each player in APNAP order, each word written under its chooser (Object.chosenNumberBy); Distinct asks only a player who chose before, offering every word but their last (Two Streams Facility).
    EachPlayer ChoiceDistinct.ChoiceDistinct
  deriving (Eq, Ord, Show)
