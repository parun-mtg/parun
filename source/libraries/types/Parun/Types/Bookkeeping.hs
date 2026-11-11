module Parun.Types.Bookkeeping where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.RepeatPicks as RepeatPicks
import qualified Parun.Types.Timestamp as Timestamp
import qualified Parun.Types.Undoing as Undoing

-- | What an instruction may write whether or not it HAPPENED (CR 603.12),
-- held whole as GameState.bookkeeping: id and timestamp supplies, interning
-- and Oracle-reference caches, and notes taken before or beside an act.
-- Parun.Engine.Resolve.Common.happenedBetween skips the whole record, so a
-- field added here is never "the board changed".
data Bookkeeping = MkBookkeeping
  { nextObjectId :: !ObjectId.ObjectId,
    nextPrintingId :: !PrintingId.PrintingId,
    -- | CR 613.7: the monotonic source of timestamps for objects and stored
    -- continuous effects. See Timestamp.
    nextTimestamp :: !Timestamp.Timestamp,
    -- | The group Event.recordEvent stamps on the next event; advanced per
    -- record except inside an Event.simultaneously bracket. Not reset at turn
    -- handoff.
    nextEventGroup :: !EventGroup.EventGroup,
    -- | How deep the Event.simultaneously brackets are nested; the outermost
    -- bracket's group wins.
    eventGroupDepth :: !Natural.Natural,
    -- | Every printing this game knows, by the id the objects carry; two ids
    -- naming one card is benign (CR 109.3). Append-only and never collected.
    printings :: !(Map.Map PrintingId.PrintingId Printing.Printing),
    -- | `printings` read backwards, so Parun.Engine.Game.intern answers with the
    -- id a printing already has.
    printingIds :: !(Map.Map Printing.Printing PrintingId.PrintingId),
    -- | CR 108.1: the printings the Oracle card reference answered a chosen
    -- name with (Prompt.LookUpCard), part of the names CR 612.7 enumerates.
    lookedUp :: !(Set.Set PrintingId.PrintingId),
    -- | CR 612.7 / 108.1: the face names the Oracle card reference answered each
    -- name grant's filter with (Prompt.ReferenceNames), asked once per filter.
    referenceNames :: !(Map.Map (Filter.Filter Keyword.Keyword) (Set.Set CardName.CardName)),
    -- | CR 104.4b: the timestamp as of the last optional action offered to a
    -- player, advanced by Parun.Engine.Game.choose (conceding excepted, CR
    -- 104.3a) and by CR 801.16's draw of a loop's players; its gap from
    -- nextTimestamp is Parun.Engine.Departure.checkMandatoryLoop's heuristic.
    lastChoice :: !Timestamp.Timestamp,
    -- | CR 733.3: Nothing outside a Parun.Types.RepeatIf; inside one, its run's
    -- picks and refusals (Parun.Engine.Resolve.Arms.Zones.repeatIf), a pick
    -- recorded before the move it names is tried.
    repeatPicks :: !(Maybe RepeatPicks.RepeatPicks),
    -- | CR 734.1: why each player's cast, activation or combat declaration
    -- was last reversed, since that player last chose an action at priority
    -- (Parun.Engine.Engine.priorityLoop) or made a declaration
    -- (Parun.Engine.Combat). Parun.Engine.PlayerView shows a seat its own.
    undone :: !(Map.Map PlayerId.PlayerId Undoing.Undoing),
    -- | CR 603.12: how many draw or mill instructions have done fewer than the
    -- number they named. It only rises, so one instruction's shortfall is a
    -- difference across it (Parun.Engine.Resolve.Common.instructionDone).
    fellShort :: !Natural.Natural
  }
  deriving (Eq, Ord, Show)
