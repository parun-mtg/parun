module Parun.Types.ColorlessOption where

-- | CR 105.2c: whether a choice of colours also offers colorless, which is no
-- colour.
data ColorlessOption
  = -- | Only colours (Gods Willing).
    ColorsOnly
  | -- | Colorless beside the colours ("colorless or the color of your choice", Angelic Intervention).
    OrColorless
  deriving (Bounded, Enum, Eq, Ord, Show)
