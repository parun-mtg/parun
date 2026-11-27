module Parun.Types.PlayerChoice where

import qualified Parun.Types.Gift as Gift
import qualified Parun.Types.KeywordInstance as KeywordInstance

-- | Which of an object's chosen players: the key of Object.chosenPlayers, and
-- what PlayerRef.ChosenPlayerOfBound asks for.
data PlayerChoice
  = -- | CR 607.2d: "the chosen player", chosen as the object entered (Stuffy
    -- Doll, CR 614.1c) or transformed (Shinryu, Transcendent Rival, CR 701.27i),
    -- or by its own resolving ability (Imprison This Insolent Wretch, ChoosePlayer.linked).
    ChosenPlayer
  | -- | CR 702.174a: the opponent this instance of this gift was promised to.
    Promised Gift.Gift KeywordInstance.KeywordInstance
  | -- | The player who secretly made the object's choice (ChooseQuality.secret): A Killer Among Us's ruling.
    SecretChooser
  deriving (Eq, Ord, Show)
