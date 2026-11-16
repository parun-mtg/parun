module Parun.Types.CastOrigin where

import qualified Parun.Types.Zone as Zone

-- | CR 601.2a: where a spell was moved to the stack from when it was cast.
data CastOrigin
  = -- | CR 400.1: one of the game's zones.
    FromZone Zone.Zone
  | -- | CR 400.11 / 707.13: outside the game, which is no zone.
    OutsideTheGame
  deriving (Eq, Ord, Show)
