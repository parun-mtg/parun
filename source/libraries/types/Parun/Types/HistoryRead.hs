module Parun.Types.HistoryRead where

import qualified Data.Set as Set
import qualified Parun.Types.PlayerAction as PlayerAction

-- | CR 733.3: which part of the turn's history -- TurnRegisters.events, or
-- TurnRegisters.activationsThisTurn -- a construct reads, as
-- Parun.Engine.Interchangeable.Mentions classifies the effect DSL and
-- Parun.Engine.Fragmented asks. Loop-local, so no codec.
data HistoryRead
  = -- | Acts of these kinds taken (CR 603.2), Parun.Types.PlayerAction.
    Acts (Set.Set PlayerAction.PlayerAction)
  | -- | Activations begun this turn, TurnRegisters.activationsThisTurn (CR 602.2).
    Activations
  | -- | Attackers declared (CR 508.1).
    Attacks
  | -- | Creatures that blocked (CR 509.3a).
    Blockers
  | -- | Attackers that became blocked (CR 509.1h).
    Blocks
  | -- | Spells cast (CR 601.2i).
    Casts
  | -- | Coins flipped (CR 705.1).
    CoinFlips
  | -- | Spells convoked (CR 702.51a).
    Convokes
  | -- | Counters put on objects (CR 122.6).
    CounterPlacements
  | -- | Vehicles crewed, and creatures crewing them (CR 702.122a).
    Crews
  | -- | Crimes committed (CR 700.13).
    Crimes
  | -- | Damage dealt (CR 120.1).
    Damage
  | -- | Permanents that died (CR 700.4).
    Deaths
  | -- | Dilemmas faced (CR 700.17).
    Dilemmas
  | -- | Cards discarded (CR 701.9a).
    Discards
  | -- | Permanents that entered the battlefield (CR 603.6a).
    Entries
  | -- | Lands played (CR 305.1).
    LandPlays
  | -- | Life gained (CR 119.3).
    LifeGains
  | -- | Life lost (CR 119.3).
    LifeLosses
  | -- | Loyalty abilities activated (CR 606.3).
    LoyaltyActivations
  | -- | Players who became the monarch (CR 726.1).
    MonarchChanges
  | -- | Cards milled (CR 701.17a).
    Mills
  | -- | Zone changes, and players leaving the game (CR 400.7, 800.4a).
    Moves
  | -- | Planar die rolls (CR 901.9).
    PlanarRolls
  | -- | Planeswalks (CR 901.11).
    Planeswalks
  | -- | Abilities that resolved (CR 608.2).
    Resolutions
  | -- | Permanents sacrificed (CR 701.21a).
    Sacrifices
  | -- | Mounts saddled, and creatures saddling them (CR 702.171a).
    Saddles
  | -- | Steps and phases that began (CR 500.1).
    StepBegins
  | -- | Permanents that became tapped (CR 701.26a).
    Taps
  | -- | Permanents tapped for mana (CR 106.12a).
    TapsForMana
  | -- | Abilities that triggered (CR 603.2).
    Triggers
  deriving (Eq, Ord, Show)
