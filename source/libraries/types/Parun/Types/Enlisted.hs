module Parun.Types.Enlisted where

import qualified Parun.Types.ObjectId as ObjectId

-- | CR 702.154c's "enlists": which creature enlisted, and which it tapped.

-- BOTH fields are an ObjectId and they are NOT interchangeable, Mentored's
-- reason for naming them: a swap would credit the wrong creature.
data Enlisted = MkEnlisted
  { enlister :: ObjectId.ObjectId,
    enlisted :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
