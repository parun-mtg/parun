-- Facts about CR 607.2n's exile before the shuffle that more than one reader
-- needs: deck legality before a game, and a game rebuilt from cards.
module Parun.Engine.PregameExile where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PregameExile as PregameExile

-- | Whether some number of these copies, each revealed, exile this many cards
-- between them (Arcane Savant's and Volatile Chimera's rulings of 2016-08-23).
admits :: PregameExile.PregameExile -> Natural.Natural -> Natural.Natural -> Bool
admits exile copies total =
  any (\revealed -> revealed * PregameExile.atLeast exile <= total && all (\most -> total <= revealed * most) (PregameExile.atMost exile)) [1 .. copies]
