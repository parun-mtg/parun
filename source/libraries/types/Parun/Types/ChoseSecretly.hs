module Parun.Types.ChoseSecretly where

import Data.Set (Set)
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.SecretChoice as SecretChoice

-- | A secret choice was made, and the players it was shown to: the chooser
-- and, per CR 724.4, the player controlling the chooser (A Killer Among Us's
-- ruling, 2024-02-02).
data ChoseSecretly = MkChoseSecretly
  { choice :: SecretChoice.SecretChoice,
    audience :: Set PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
