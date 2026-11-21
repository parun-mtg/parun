module Parun.Types.TokenLot where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.Card as Card
import qualified Parun.Types.ShownDie as ShownDie
import qualified Parun.Types.TokenCopy as TokenCopy

-- | CR 111.1: some number of tokens of one shape, as one creation event would
-- make them. A Parun.Types.ProposedEvent.WouldCreateTokens carries a sequence
-- of these: one lot per effect until a replacement appends another
-- (Parun.Types.TokenR.plus).
data TokenLot = MkTokenLot
  { card :: Card.Card,
    -- | CR 707.1's copy token: what it copies off the copied permanent.
    -- Nothing for a token whose text is given.
    copy :: Maybe TokenCopy.TokenCopy,
    count :: Natural.Natural,
    -- | CR 614.16: an effect makes this lot -- a resolving spell's or
    -- ability's, or another replacement effect's. False for CR 118.12's cost,
    -- which is no effect (Library of Leng's ruling).
    byEffect :: Bool,
    -- | The die each token of this lot IS (Parun.Types.Create's `die`), which
    -- Parun.Engine.Game.tokenOfLot stamps on it. Nothing for a lot a
    -- replacement appended or swapped in: a different token, no die behind it.
    die :: Maybe (ShownDie.ShownDie Natural.Natural)
  }
  deriving (Eq, Ord, Show)
