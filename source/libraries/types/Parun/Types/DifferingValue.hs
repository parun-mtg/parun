module Parun.Types.DifferingValue where

import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.PlayerId as PlayerId

-- | One value a card holds on a Parun.Types.DifferentIn axis, which a group
-- kept apart may hold at most once.
data DifferingValue
  = -- | A name (CR 201.2b).
    MkName CardName.CardName
  | -- | A power, toughness or mana value (CR 208.1, 202.3).
    MkNumber Integer
  | -- | A card type (CR 205.2a).
    MkCardType CardType.CardType
  | -- | A controller (CR 110.2).
    MkPlayer PlayerId.PlayerId
  deriving (Eq, Ord, Show)
