module Parun.Types.PlayerChoice where

import qualified Parun.Types.Gift as Gift
import qualified Parun.Types.KeywordInstance as KeywordInstance

-- | Which of an object's chosen players: the key of Object.chosenPlayers, and
-- what PlayerRef.ChosenPlayerOfBound asks for.
data PlayerChoice
  = -- | CR 614.1c / 701.27i: the player chosen as the object entered (Stuffy
    -- Doll) or transformed (Shinryu, Transcendent Rival).
    AsEntersOrTransforms
  | -- | CR 702.174a: the opponent this instance of this gift was promised to.
    Promised Gift.Gift KeywordInstance.KeywordInstance
  | -- | CR 702.106b: the player who secretly made the object's choice (ChooseQuality.secret).
    SecretChooser
  deriving (Eq, Ord, Show)
