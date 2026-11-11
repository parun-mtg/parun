module Parun.Types.Exponentiation where

-- | The payload of Parun.Types.Arithmetic's Exponentiation arm: a base raised to an
-- exponent (CR 107.1), which is how Mathemagics' "2ˣ cards" reads.
--
-- PARAMETRIC in the quantity for Parun.Types.Quotient's reason: each operand is
-- a whole Quantity and Quantity names this record.
data Exponentiation quantity = MkExponentiation
  { base :: quantity,
    exponent :: quantity
  }
  deriving (Eq, Ord, Show)
