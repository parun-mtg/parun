module Parun.Types.MatchScope where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.GameSettings as GameSettings

-- | CR 100.6a: what belongs to the match rather than to one game of it, held
-- whole as GameState.match. Parun.Engine.Setup's restartGame (CR 728.1) and
-- subgameStateFrom (CR 730.2) carry the whole record into the game they build,
-- so a field added here survives both.
data MatchScope = MkMatchScope
  { -- | CR 800.2: the options this game was started with; never written after
    -- the game begins.
    settings :: !GameSettings.GameSettings,
    -- | CR 100.6a / 730.1a: how many subgames have begun this match, counting
    -- one in progress. CR 730.1b keeps a subgame's effects out of the main
    -- game, but a match is a series of games, so this is inherited by the
    -- subgame and kept across CR 728.1's restart, which begins a new game of
    -- the SAME match. Parun models no match beyond it, so a game built by
    -- Setup.emptyGame starts at 0.
    subgamesThisMatch :: !Natural.Natural,
    -- | CR 728.1 / 730.1a: how many games have begun this match, counting this
    -- one, every subgame and every restarted game.
    gamesThisMatch :: !Natural.Natural,
    -- | CR 728.1 / 730.1a: which of `gamesThisMatch` this game is, so a
    -- subgame's or a restarted game's turn numbers, which begin again at 1,
    -- are not taken for this game's. Carried only for startGameFromCards to
    -- number the new game. Parun.Engine.PlayerView shows it.
    gameNumber :: !Natural.Natural
  }
  deriving (Eq, Ord, Show)
