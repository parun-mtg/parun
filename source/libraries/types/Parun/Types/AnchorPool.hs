module Parun.Types.AnchorPool where

-- | Which objects a Parun.Types.Anchor.AnyObject group ranges over.
data AnchorPool
  = -- | CR 109.2: the permanents on the battlefield (Pattern Matcher).
    Permanents
  | -- | CR 406.1: the cards in exile (Cemetery Illuminator).
    ExiledCards
  deriving (Bounded, Enum, Eq, Ord, Show)
