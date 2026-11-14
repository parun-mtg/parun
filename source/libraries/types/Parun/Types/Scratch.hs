module Parun.Types.Scratch where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Arrival as Arrival
import qualified Parun.Types.Cause as Cause
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.EnteringTogether as EnteringTogether
import qualified Parun.Types.EntryOrigin as EntryOrigin
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.ThrowResult as ThrowResult

-- | What is meaningful only while something resolves, enters, is announced or
-- is checked, held whole as GameState.scratch: each field is empty, Nothing or
-- False at every priority window. Its fields open and close on different
-- windows, so each keeps its own emptiness rather than the record being one
-- Maybe. Parun.Engine.Setup's restartGame (CR 728.1) and subgameStateFrom (CR
-- 730.2) begin it again.
data Scratch = MkScratch
  { -- | CR 613.7m: the arrivals of the CR 608.2f action under way, ordered when
    -- it ends (Event.together). Nothing outside one.
    enteringTogether :: !(Maybe EnteringTogether.EnteringTogether),
    -- | CR 401.4 / 404.3: the redirected library arrivals and the graveyard
    -- arrivals of the Event.arrivingTogether scope under way, arranged by their
    -- owners when it ends. Nothing outside one.
    arrivals :: !(Maybe (Seq.Seq Arrival.Arrival)),
    -- | CR 118.12: the instructions running are the "[do something]" a later
    -- clause's "If [a player] does" reads, so a cost paid on resolution rather
    -- than an effect (Event.payingOnResolution). False outside one.
    payingOnResolution :: !Bool,
    -- | CR 609.1: the spell or ability whose effect is being
    -- performed (Event.causedBy), which Moved and Discarded record as their
    -- cause. Nothing outside one, and inside a cost's payment outside a
    -- resolution (Parun.Engine.Cost.payComponent).
    causing :: !(Maybe Cause.Cause),
    -- | CR 616.1 and Lich's Mirror's ruling: the objects a state-based action
    -- check moves, keyed to the source of the replacement whose effects run in
    -- place of a loss in that check, or to Nothing once kept for the check. A
    -- move those effects make of one asks its controller which of the two
    -- happens (Event.contestedKept). Empty outside that window.
    contested :: !(Map.Map ObjectId.ObjectId (Maybe ObjectId.ObjectId)),
    -- | The amount bindings read outside any object's environment: CR 615.5's
    -- prevention rider, its recipient possibly being a player, and what a
    -- mana ability's cost paid (CR 605.3b). Empty at every priority window.
    ambientAmounts :: !(Map.Map SlotName.SlotName Natural.Natural),
    -- | CR 113.6 / 614.12: the permanents entering beside the one whose entry
    -- loop is running -- materialized but not yet entered, so their static
    -- abilities do not function. Never holds the loop's own subject. Also held
    -- while the subject's as-enters effects run
    -- (Parun.Engine.Resolve.Effect.runEntryEffect), and a resolution's
    -- battlefield sweep skips it. Empty at every priority window.
    enteringBeside :: !(Set.Set ObjectId.ObjectId),
    -- | CR 614.12: the subject of every entry loop currently running,
    -- materialized but not entered, with the zone it left, if any; with
    -- `enteringBeside`, what Parun.Engine.Projection.boardAsEntering subtracts.
    -- Empty at every priority window.
    enteringSubjects :: !(Map.Map ObjectId.ObjectId (Maybe EntryOrigin.EntryOrigin)),
    -- | CR 614.12 / 608.2f: the zone each permanent that has entered so far in
    -- the Event.arrivingTogether scope under way left, so a later member's
    -- entry still counts it there (Parun.Engine.Projection.boardAsEntering).
    -- Empty outside one.
    enteredFrom :: !(Map.Map ObjectId.ObjectId EntryOrigin.EntryOrigin),
    -- | CR 614.12b: the members of a CR 608.2f move onto the battlefield that
    -- have not yet been moved, each with the player it will enter under, which
    -- the one-at-a-time funnel would otherwise hide from an earlier member's
    -- entry choice. Empty at every priority window.
    enteringPending :: !(Map.Map ObjectId.ObjectId PlayerId.PlayerId),
    -- | CR 614.1a: while a land play is moving its card, the pre-move ids whose
    -- entry an EntryRewrite.SacrificeToEnter turned into the graveyard -- the
    -- land was still played (CR 305.1). Nothing when no play is listening,
    -- which is at every priority window.
    refusedEntries :: !(Maybe (Set.Set ObjectId.ObjectId)),
    -- | CR 614.1c: the counters each entering permanent is so far going to
    -- enter with, by the player putting them -- Nothing for CR 122.6a's
    -- default, the object's controller, and Just for a player the effect names
    -- (CR 702.104a's opponent) -- pending until runEntry flushes them onto the
    -- object. Empty outside an entry.
    enteringCounters :: !(Map.Map ObjectId.ObjectId (Map.Map (Maybe PlayerId.PlayerId) (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural))),
    -- | CR 707.9e: the share of each entering permanent's enteringCounters that
    -- the copy effect applied to it last placed as its exception, which a later
    -- copy effect on the same entry takes back. Always CR 122.6a's default
    -- putter's share. Empty outside an entry.
    copyExceptionCounters :: !(Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural)),
    -- | CR 121.8: Nothing unless a spell is being cast or an ability activated
    -- (Parun.Engine.Event.announcing); then the cards drawn since it began,
    -- each kept face down in its hand with no characteristics, keyed to the
    -- ordinal GameEvent.Drew stamped on its draw.
    keptFaceDown :: !(Maybe (Map.Map ObjectId.ObjectId Natural.Natural)),
    -- | CR 401.5: Nothing unless a spell is being cast or an ability activated
    -- (Parun.Engine.Event.announcing); then each library's top card as it
    -- began, the only one that stays shown until it ends (Game.shownTop).
    announcedTops :: !(Maybe (Map.Map PlayerId.PlayerId ObjectId.ObjectId)),
    -- | CR 401.5: Nothing unless a player is taking a special action
    -- (Parun.Engine.Event.takingSpecialAction); then each library's top card
    -- as it began, the only one that stays shown until it ends
    -- (Game.shownTop). Apart from announcedTops, whose window a mana ability
    -- activated during the action opens and closes inside this one.
    actionTops :: !(Maybe (Map.Map PlayerId.PlayerId ObjectId.ObjectId)),
    -- | CR 101.4b: Nothing outside a Parun.Types.ForEach loop; inside one, the
    -- permanents each ChoosePermanents of it has picked so far, beside who
    -- picked them, which a later pick's prompt carries
    -- (Parun.Engine.Resolve.Arms.Zones.forEach).
    loopPicks :: !(Maybe (Seq.Seq (PlayerId.PlayerId, Set.Set ObjectId.ObjectId))),
    -- | CR 706.2 / 705.1: what the die whose modification step is open, or the
    -- coin just flipped, came up, while Parun.Engine.Resolve.Effect offers to
    -- throw it again; Nothing otherwise.
    throwing :: !(Maybe ThrowResult.ThrowResult),
    -- | CR 706.2b: whether an ability activated inside that window asked for
    -- the die to be rerolled or the coin reflipped (Goblin Bookie).
    rethrowRequested :: !Bool
  }
  deriving (Eq, Ord, Show)
