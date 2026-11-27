module Parun.Types.WhichTurn where

-- | CR 500.1 / 608.2i: which turn's history a tally reads -- the turn in progress, the
-- one before it, whoever's it was ("last turn", Feast on the Fallen), or every turn
-- since the perspective's own last one.
data WhichTurn
  = -- | The turn in progress, TurnRegisters.events.
    ThisTurn
  | -- | The turn before it, the newest of History.pastTurns: empty on the
    -- game's first turn.
    LastTurn
  | -- | CR 608.2i: every turn since the perspective's own most recent turn ended
    -- (Game.ownLastTurn), the live one included -- Marchesa, Resolute Monarch's
    -- "since your last turn".
    --
    -- Unprinted: a Parun.Types.LifeTally or a draw tally reading this window;
    -- Parun.EffectLintSpec's "no card writes a draw or life tally since your
    -- last turn" keeps it out of card data.
    SinceOwnLastTurn
  deriving (Bounded, Enum, Eq, Ord, Show)
