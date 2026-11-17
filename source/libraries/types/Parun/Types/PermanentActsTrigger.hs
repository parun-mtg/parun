module Parun.Types.PermanentActsTrigger where

import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PermanentActed as PermanentActed

-- | CR 603.2: which permanent's action fires the ability, what the card the act
-- revealed must be (Nicanzil, Current Conductor's "explores a land card"; no
-- Filter asks nothing), and per permanent or per batch (CR 603.2c) -- Teferi's
-- Imp's "whenever this creature phases out", The War Doctor's "whenever one or
-- more other permanents phase out".
data PermanentActsTrigger = MkPermanentActsTrigger
  { acted :: PermanentActed.PermanentActed (Maybe (Filter.Filter Keyword.Keyword)) ActingPermanent.ActingPermanent,
    batching :: Batching.Batching
  }
  deriving (Eq, Ord, Show)
