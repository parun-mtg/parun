module Parun.Types.PermanentActsTrigger where

import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.PermanentActed as PermanentActed

-- | CR 603.2: which permanent's action fires the ability, and per permanent or
-- per batch (CR 603.2c) -- Teferi's Imp's "whenever this creature phases out",
-- The War Doctor's "whenever one or more other permanents phase out".
data PermanentActsTrigger = MkPermanentActsTrigger
  { acted :: PermanentActed.PermanentActed ActingPermanent.ActingPermanent,
    batching :: Batching.Batching
  }
  deriving (Eq, Ord, Show)
