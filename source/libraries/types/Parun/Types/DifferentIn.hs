module Parun.Types.DifferentIn where

-- | A characteristic no two cards a search finds may share (CR 701.23a's
-- "matches the given description", said of the found group as a whole), no
-- two cards a wish reveals (Parun.Types.FromOutsideTheGame), no two picks from
-- among a group (Parun.Types.ChosenCardFromAmong) or a shared zone
-- (Parun.Types.ChosenPermanents), no two targets of a slot
-- (Parun.Types.Together), or no two permanents a cost sacrifices
-- (Parun.Types.Sacrifice).
data DifferentIn
  = -- | Gifts Ungiven's "with different names" (CR 201.2b).
    Names
  | -- | Threats Undetected's "with different powers" (CR 208.1).
    Powers
  | -- | The Karst, Enchanted's "don't share a mana value" (CR 202.3).
    ManaValues
  | -- | The Karst, Enchanted's "don't share ... toughness" (CR 208.1).
    Toughnesses
  | -- | The Karst, Enchanted's "don't share ... card type" (CR 205.2a).
    CardTypes
  | -- | Cloud's Limit Break's "with different controllers" (CR 110.2).
    Controllers
  deriving (Bounded, Enum, Eq, Ord, Show)
