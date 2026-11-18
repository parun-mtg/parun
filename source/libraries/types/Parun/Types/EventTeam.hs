module Parun.Types.EventTeam where

-- | A team a player joins for an event outside the game, which a playtest
-- card's paragraph heading names ("Team Cloudspire —"). Not CR 808.1's team
-- (Parun.Types.TeamId): the CR never mentions these. Player.eventTeam holds a
-- player's (docs/decisions.md, "Where a player's event team comes from").
data EventTeam
  = -- | Team Cloudspire, as Now THIS Is Aether Racing prints it.
    Cloudspire
  | -- | Team Speed Demons, as Now THIS Is Aether Racing prints it.
    SpeedDemons
  deriving (Bounded, Enum, Eq, Ord, Show)
