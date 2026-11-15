module Parun.Types.ShownCards where

-- | CR 401.5 / 400.2 / 708.5: which of a player's hidden cards a standing grant
-- shows.
data ShownCards
  = -- | CR 400.2: every card in the player's hand.
    Hand
  | -- | CR 401.5: the top card of the player's library.
    TopOfLibrary
  | -- | CR 708.5: each face-down creature the player controls.
    FaceDownCreatures
  deriving (Bounded, Enum, Eq, Ord, Show)
