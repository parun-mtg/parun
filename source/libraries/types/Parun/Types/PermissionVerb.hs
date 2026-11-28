module Parun.Types.PermissionVerb where

-- | CR 601.3 / 305.1: what a CR 601.3 permission lets a player do with a card it
-- covers. Garruk's Horde's "you may CAST creature spells" is Cast; Serra
-- Paragon's "you may PLAY a land ... or CAST a permanent spell", which is the
-- glossary's "play" (a land played or a spell cast), is Play; Mosswood
-- Dreadknight's "you may cast it ... AS AN ADVENTURE" is CastAsAdventure; The
-- Omenkeel's "you may play LANDS from among those cards" is PlayLand.
--
-- A field of the permission rather than a second arm beside
-- Parun.Types.PlayerEffect's PlayLandsFrom, because a Play permission is ONE
-- permission: its once-each-turn budget (Parun.Types.PermissionLimit) is spent
-- by a land play or a cast, whichever comes first, and two arms would keep two.
data PermissionVerb
  = -- | CR 601.3: the covered card may be cast, and never played as a land.
    Cast
  | -- | CR 601.3 / 305.1: the covered card may be cast, or played if it is a land.
    Play
  | -- | CR 601.3 / 716.3: the covered card may be cast as an Adventure only.
    CastAsAdventure
  | -- | CR 305.1 / 712.9: the covered card may be played as a land, by any land face, and never cast.
    PlayLand
  deriving (Bounded, Enum, Eq, Ord, Show)
