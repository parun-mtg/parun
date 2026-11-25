module Parun.Types.SeatView where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.DraftNote as DraftNote
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerDesignation as PlayerDesignation
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Status as Status
import qualified Parun.Types.TeamId as TeamId

-- | CR 400.2: one player's zones and totals as one seat sees them, a hidden
-- zone by its size and only the members that seat may see.
data SeatView = MkSeatView
  { player :: PlayerId.PlayerId,
    -- | CR 800.4a: whether the player is still in the game or has left it.
    status :: Status.Status,
    -- | CR 808.1: the team the player is on, Nothing in a game not played
    -- between teams (CR 102.4).
    team :: Maybe TeamId.TeamId,
    -- | CR 810.4: the team's shared total where its life is shared
    -- (Parun.Engine.Game.lifeSharers keeps each member's equal to it).
    life :: Integer,
    counters :: Map.Map PlayerCounterKind.PlayerCounterKind Natural.Natural,
    handSize :: Natural.Natural,
    hand :: [ObjectView.ObjectView],
    librarySize :: Natural.Natural,
    library :: [ObjectView.ObjectView],
    graveyard :: [ObjectView.ObjectView],
    exile :: [ObjectView.ObjectView],
    manaPool :: Mana.Mana,
    -- | CR 701.54c: how many times the Ring has tempted the player.
    ringTemptations :: Natural.Natural,
    -- | CR 702.179b: the player's speed, Nothing while they have none.
    speed :: Maybe Natural.Natural,
    -- | CR 702.131c / 702.195b: the city's blessing and the enduring story.
    designations :: Set.Set PlayerDesignation.PlayerDesignation,
    -- | CR 309.7: how many dungeons the player has completed.
    completedDungeons :: Natural.Natural,
    -- | CR 905.2b: Player.draftNotes, which every seat may look at.
    draftNotes :: Map.Map CardName.CardName (Seq.Seq DraftNote.DraftNote),
    -- | CR 903.10a: the combat damage each commander has dealt the player, by the seat whose deck designated it (CR 903.3) and its name.
    commanderDamage :: Map.Map PlayerId.PlayerId (Map.Map CardName.CardName Natural.Natural)
  }
  deriving (Eq, Show)
