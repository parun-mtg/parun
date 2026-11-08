module Parun.Types.Printing where

import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Card as Card
import qualified Parun.Types.Color as Color

data Printing = MkPrinting
  { card :: Card.Card,
    -- | CR 718.1: the numbers lit up on an Attraction, empty for every other
    -- card. Printing data rather than a characteristic (CR 109.3): two
    -- printings of one Attraction light different numbers.
    lights :: Set.Set Natural.Natural,
    -- | CR 207.5: the colours circled on this copy of Cryptic Spires as its
    -- deck was built, empty for every other card. Printed rules text (CR
    -- 613.1), so Parun.Engine.Projection.View seeds it and a copy takes it.
    circled :: Set.Set Color.Color,
    -- | CR 903.4b: the colour chosen for this copy as its deck's commander
    -- (The Prismatic Piper), Nothing for every other card. Not copiable (CR
    -- 607.5a), so Parun.Engine.Commander.chosenColorOf reads it off the
    -- commander alone.
    chosenColor :: Maybe Color.Color
  }
  deriving (Eq, Ord, Show)

-- | A printing with no lights, nothing circled and no colour chosen, which is
-- every card but an Attraction, Cryptic Spires and The Prismatic Piper.
ofCard :: Card.Card -> Printing
ofCard c = MkPrinting {card = c, lights = Set.empty, circled = Set.empty, chosenColor = Nothing}
