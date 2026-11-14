module Parun.Types.TopOfLibraryUntil where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SideDeck as SideDeck

-- | The top of a library (CR 401.2, CR 121.1) walked down to and INCLUDING the
-- card whose match ENDS the walk, named by whose library, by what a match is,
-- by what each match adds to the tally, and by the tally that ends it: Treasure
-- Hunt's "reveal cards from the top of your library until you reveal a nonland
-- card" is one match weighing 1, Open the Way's "until you reveal X land cards"
-- is X of them, and Improvisation Capstone's "until you exile cards with total
-- mana value 4 or greater" is every card weighing its mana value (CR 202.3)
-- until the sum reaches 4.
--
-- Parun.Types.TopOfLibrary carries the same `player` and `count` and no Filter,
-- and that Filter is the whole difference between them: both name a prefix of
-- the pile from its head, one measured in CARDS and one in MATCHES. The Filter is
-- matched against each card's own projection as the walk reaches it (CR 608.2c),
-- so a library holding fewer matches than the count gives up all of it (CR
-- 609.3).
--
-- The count is a Quantity for TopOfLibrary's reason: Open the Way's is CR
-- 601.2b's announced X, read when the effect executes (CR 608.2h), and an
-- unevaluable or negative one is clamped to zero there.
--
-- The fields are named rather than positional, the shape every other
-- Parun.Types.ObjectRef payload record takes.
data TopOfLibraryUntil = MkTopOfLibraryUntil
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword,
    -- | What one match adds to the tally, read against that card's projection;
    -- an unevaluable or negative weight adds nothing (CR 107.2, 107.1b).
    weight :: Quantity.Quantity,
    count :: Quantity.Quantity,
    -- | Nothing for the library; a side deck's top otherwise (CR 901.3 / 311.7:
    -- The Fertile Lands of Saulvinia's "until you reveal a plane card").
    deck :: Maybe SideDeck.SideDeck
  }
  deriving (Eq, Ord, Show)
