module Parun.Types.BlockTrigger where

import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.BlockCount as BlockCount
import qualified Parun.Types.BlockRole as BlockRole
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | CR 509.3: "whenever [a creature] blocks" (BlockRole.Blocker) or "becomes
-- blocked" (BlockRole.Attacker), the Filter naming that creature
-- (Filter.IsSource for the source's own), how the other side is counted, and
-- per creature or per batch (CR 603.2c) -- Neyith of the Dire Hunt's "whenever
-- one or more creatures you control ... become blocked".
data BlockTrigger = MkBlockTrigger
  { role :: BlockRole.BlockRole,
    creature :: Filter.Filter Keyword.Keyword,
    count :: BlockCount.BlockCount,
    batching :: Batching.Batching
  }
  deriving (Eq, Ord, Show)
