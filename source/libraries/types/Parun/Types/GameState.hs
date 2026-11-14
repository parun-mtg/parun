module Parun.Types.GameState where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.Binding as Binding
import qualified Parun.Types.Bookkeeping as Bookkeeping
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CounterLink as CounterLink
import qualified Parun.Types.Daytime as Daytime
import qualified Parun.Types.EntryLink as EntryLink
import qualified Parun.Types.ExileLink as ExileLink
import qualified Parun.Types.ExtraTurn as ExtraTurn
import qualified Parun.Types.History as History
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OutsideCard as OutsideCard
import qualified Parun.Types.OutsideObject as OutsideObject
import qualified Parun.Types.PhasedOut as PhasedOut
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerControl as PlayerControl
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.RestartSignal as RestartSignal
import qualified Parun.Types.Result as Result
import qualified Parun.Types.ReturnWatch as ReturnWatch
import qualified Parun.Types.Scratch as Scratch
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Snapshots as Snapshots
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.Timestamp as Timestamp
import qualified Parun.Types.TurnMarker as TurnMarker
import qualified Parun.Types.TurnRegisters as TurnRegisters

data GameState = MkGameState
  { -- | CR 100.6a: what crosses a restart and a subgame (Parun.Types.MatchScope).
    match :: MatchScope.MatchScope,
    objects :: Map.Map ObjectId.ObjectId Object.Object,
    library :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    hand :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    graveyard :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    -- | CR 400.1's battlefield, minus its phased-out permanents, which
    -- `phasedOut` holds instead.
    battlefield :: Set.Set ObjectId.ObjectId,
    -- | CR 702.26b: the phased-out permanents, each with the player who
    -- controlled it when it phased out (CR 702.26a) and which of rule 702.26's
    -- schedules it is on. A separate set so every battlefield reader treats
    -- them as not existing by construction; see Parun.Types.PhasedOut.
    phasedOut :: Map.Map ObjectId.ObjectId PhasedOut.PhasedOut,
    exile :: Set.Set ObjectId.ObjectId,
    -- | CR 400.1: the command zone, shared rather than per-player.
    command :: Set.Set ObjectId.ObjectId,
    -- | CR 400.1 / 407: the ante zone, shared rather than per-player.
    ante :: Set.Set ObjectId.ObjectId,
    -- | CR 718.2 / 901.3 / 904.3 and the Unstable FAQ: each player's
    -- supplementary decks, keyed by which deck (Parun.Types.SideDeck), head on
    -- top. Their cards are in the command zone (Object.zone, CR 718.2 / 901.4 /
    -- 904.4) but not in `command`, which holds what is face up there. A player
    -- keyed with an empty deck is one who brought that deck
    -- (Parun.Engine.Archenemy.isArchenemy). Parun.Engine.Game.overSideDecks drops
    -- a kind nobody holds, so a board that never had one compares equal to one
    -- that has emptied it.
    sideDecks :: Map.Map SideDeck.SideDeck (Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId)),
    -- | CR 500: the turn being worked; in Grand Melee the others running are
    -- on `parkedTurns` (CR 807.4b).
    turn :: TurnRegisters.TurnRegisters,
    players :: Map.Map PlayerId.PlayerId Player.Player,
    -- | CR 730.4: the cards outside this game that sit in a game on hold; empty
    -- for a game nobody is nested inside.
    outsideObjects :: Map.Map ObjectId.ObjectId OutsideObject.OutsideObject,
    -- | CR 730.4a: the outer ids this game has brought in, in crossing order,
    -- read by Parun.Engine.Setup.applyCrossings.
    broughtIn :: Seq.Seq ObjectId.ObjectId,
    -- | CR 800.4a / 407.3: each card that left the game with a departed owner
    -- who did not begin the game owning it, as it left, keyed by its last id
    -- (Parun.Engine.Departure.objectsLeaveWith). Grows for the whole game.
    departedCards :: Map.Map ObjectId.ObjectId Object.Object,
    -- | CR 106.4. Absent from the map means an empty pool.
    manaPool :: Map.Map PlayerId.PlayerId Mana.Mana,
    -- | What one resolution, entry, announcement or check holds while it runs
    -- (Parun.Types.Scratch).
    scratch :: Scratch.Scratch,
    -- | CR 611.2 / 603.7 / 614: every row a resolution stored, kind by kind
    -- (Parun.Engine.Stored walks them).
    stored :: StoredEffects.StoredEffects,
    -- | CR 603.12: what an instruction may write whether or not it happened
    -- (Parun.Types.Bookkeeping).
    bookkeeping :: Bookkeeping.Bookkeeping,
    -- | CR 603.10 / 608.2h: the game as it stood at earlier moments
    -- (Parun.Types.Snapshots).
    snapshots :: Snapshots.Snapshots GameState,
    -- | CR 730.5: the slots a resolution filled after its own object ceased to
    -- exist, keyed by that object's id; the fallback Parun.Engine.Resolve reads
    -- for an object it can no longer find. Never pruned.
    detachedBindings :: Map.Map ObjectId.ObjectId (Map.Map SlotName.SlotName Binding.Binding),
    -- | The seating order (CR 800.5) rotated so the starting player is first,
    -- which CR 103.1 makes the turn order until a reversal
    -- (`turnOrderReversed`). Lists every player who began the game and is
    -- never shortened -- CR 800.4a and 800.4m need a departed player's seat;
    -- who is still in is Game.stillPlaying. Turn.turnOrder reads it as the
    -- turn order.
    seating :: [PlayerId.PlayerId],
    -- | CR 101.4 with Aeon Engine's rulings: whether the turn order now runs
    -- opposite to `seating`, an odd number of reversals having resolved.
    -- Seating itself does not change.
    turnOrderReversed :: Bool,
    -- | CR 500.7 / 805.4: how many turns each player has begun this game, extra
    -- turns included; under the shared team turns option a team's turn is each
    -- member's. Kept across turns; a restart (CR 728.1) and a subgame (CR
    -- 730.2) begin it again. Turn.turnsBegun reads it.
    turnsBegun :: Map.Map PlayerId.PlayerId Natural.Natural,
    result :: Maybe Result.Result,
    -- | CR 728.4: raised while a restart has replaced this game underneath the
    -- frames still running it; Engine.runStep lowers it as turn 1's untap step
    -- begins.
    restartSignal :: RestartSignal.RestartSignal,
    -- | CR 400.11 / 707.13: objects in `objects` that are outside the game, in no
    -- zone's membership. Parun.Engine.Game.zoneOf answers Nothing for them.
    outsideCopies :: Set.Set ObjectId.ObjectId,
    -- | CR 400.11 / 701.23j: the card outside the game each of those objects
    -- stands for, where a search revealed it there and left it for a later
    -- clause of the same resolution to bring in (Event.holdFound).
    outsideFinds :: Map.Map ObjectId.ObjectId OutsideCard.OutsideCard,
    -- | CR 707.13: the card names each object's ability has chosen to copy by
    -- name. Keyed by id, so CR 400.7's new object remembers none.
    namedCopyChoices :: Map.Map ObjectId.ObjectId (Set.Set CardName.CardName),
    -- | CR 707.14: the card whose name an effect noted as each object entered
    -- (EntryRiders.noted). Keyed by id, so neither CR 400.7's new object nor a
    -- copy of the object knows it.
    notedCards :: Map.Map ObjectId.ObjectId PrintingId.PrintingId,
    -- | CR 607.2e: the type and amount of mana an ability noted for each object
    -- (Effect.NoteManaSpent), the last note replacing the one before. Keyed by
    -- id, so CR 400.7's new object knows none (Ice Cauldron).
    notedMana :: Map.Map ObjectId.ObjectId (Seq.Seq ManaType.ManaType),
    -- | CR 801.16: for each player, nextTimestamp as a triggered ability of
    -- theirs, or one an effect of theirs made trigger, was last put on the
    -- stack, or, under limited range, an event last acted on an object of
    -- theirs (Parun.Engine.Game.involve).
    -- Parun.Engine.Departure.checkMandatoryLoop reads the recent ones as the loop's
    -- players.
    loopInvolvement :: Map.Map PlayerId.PlayerId Timestamp.Timestamp,
    drewFromEmpty :: Set.Set PlayerId.PlayerId,
    -- | CR 305.2a: how many lands each player has played this turn, a count
    -- because CR 305.2 lets an effect raise the allowance; cleared per player
    -- at their untap step.
    landsPlayed :: Map.Map PlayerId.PlayerId Natural.Natural,
    -- | CR 305.1 / 601.2a: each card played this game, keyed by the id it had
    -- where it was played from and by the one the play gave it (CR 400.7h,
    -- 400.7i), naming who played it. Never cleared: a delayed trigger may ask on
    -- a later turn (CR 603.7), and CR 400.7 never reuses an id.
    cardsPlayed :: Map.Map ObjectId.ObjectId PlayerId.PlayerId,
    -- | CR 401.6: each library's top card as the last settle found it revealed
    -- to every player by a player effect (Event.settleRevealedTops).
    revealedTops :: Map.Map PlayerId.PlayerId ObjectId.ObjectId,
    -- | CR 801.2c: the players who have left the game since this turn began,
    -- whose seats still count toward range of influence until the next turn
    -- begins; cleared at turn handoff.
    departedThisTurn :: Set.Set PlayerId.PlayerId,
    -- | CR 724.1: pending player-controlling effects, keyed by the player to be
    -- controlled, each promoted into `control` as the turn or combat phase its
    -- duration names begins (CR 724.1b) with the timestamp of its creation.
    --
    -- At most one row per duration, the last created (CR 724.1a), and one per
    -- duration rather than one per player for `control`'s reason: a combat
    -- phase's control scheduled after a whole turn's must hand that turn back.
    -- Never empty.
    pendingControl :: Map.Map PlayerId.PlayerId (NonEmpty.NonEmpty PlayerControl.PlayerControl),
    -- | CR 724: who is being controlled right now, keyed by the controlled
    -- player. Keyed rather than a single slot for the active player because CR
    -- 724.2's control is not the active player's and does not last a turn
    -- (see #881); each row carries its own lifetime, and CR 724.3 is why a row
    -- says nothing about who the active player is.
    --
    -- A STACK per player, oldest first, because CR 724.1a's "overwrite" is the
    -- continuous-effect sense: both effects exist and the last one CREATED is
    -- the one that works, so a rule 724.2 control laid over a live rule 724.1
    -- one must give that one back when it lapses; see #3351. Never empty -- a
    -- player with no control has no key.
    control :: Map.Map PlayerId.PlayerId (NonEmpty.NonEmpty PlayerControl.PlayerControl),
    -- | CR 726.1 / 726.3: the monarch, a single game-wide designation.
    monarch :: Maybe PlayerId.PlayerId,
    -- | CR 727.1 / 727.3: the initiative, the monarch's sibling designation.
    initiative :: Maybe PlayerId.PlayerId,
    -- | CR 732.1: the game's day\/night designation; Nothing is the rule's "neither",
    -- unreachable once either has been gained. Written only by
    -- Parun.Engine.Daytime.
    daytime :: Maybe Daytime.Daytime,
    -- | CR 608.2i: what this game did that outlives the turn's log
    -- (Parun.Types.History).
    history :: History.History,
    -- | CR 615.5 / 615.13: how much damage each object's printed prevention
    -- effect has stopped this game, keyed by the DamagePrevented.by's printer.
    -- Never cleared: a next end step can come on a later turn, and CR 400.7
    -- never reuses an id.
    damagePreventedBy :: Map.Map ObjectId.ObjectId Natural.Natural,
    -- | CR 610.3: objects moved "until" a specified event, keyed by the
    -- incarnation the move minted, swept by Parun.Engine.MoveDuration.returnDue.
    -- Not an Expiry, which cannot perform a zone change.
    movedUntil :: Map.Map ObjectId.ObjectId ReturnWatch.ReturnWatch,
    -- | CR 702.55b: which object each haunting card haunts, keyed by the exiled
    -- incarnation; the value is never cleaned up, the haunted creature dying
    -- being the point.
    haunting :: Map.Map ObjectId.ObjectId ObjectId.ObjectId,
    -- | CR 702.99b: the creature each card with cipher is encoded on, keyed by
    -- the exiled incarnation; CR 702.99c's persistence is read, not swept.
    encoded :: Map.Map ObjectId.ObjectId ObjectId.ObjectId,
    -- | CR 607.2's linked set as a relation: the object each exiled card is
    -- linked to, and the name of the ability that exiled it (CR 607.2a), keyed
    -- by the exiled incarnation and written by
    -- Parun.Engine.Resolve.Effect.applyEffectWith (rule 607.2a) and Parun.Engine.Event's
    -- zone change funnel (rule 607.2b) as a difference over GameState.exile,
    -- never a case over the opcode. Rule 614.14's link has a third writer,
    -- Parun.Engine.Event's EntryRewrite.ExileFromGraveyard arm, which names the
    -- entering permanent outright: no diff can find it, an entry replacement
    -- being neither an opcode's window nor a redirected move. Cleaned up by key
    -- only.
    exiledWith :: Map.Map ObjectId.ObjectId ExileLink.ExileLink,
    -- | CR 400.7 / 607.5: the objects whose effects put each permanent onto the
    -- battlefield or assimilated it (CR 701.72c), and the acquisition each
    -- ability came with, keyed by the incarnation and written through
    -- Parun.Engine.Resolve.Common.linkEntrant: by Parun.Engine.Resolve.Arms.Zones's
    -- Effect.MoveToZone arm, by Parun.Engine.Resolve.Arms.Objects's for a
    -- Create.linked or CreateCopy.linked token, and by
    -- Parun.Engine.Resolve.Arms.Counters's Effect.Assimilate arm. A SET, since
    -- a permanent that entered with one source can be assimilated with another.
    -- Read by Filter.EnteredWithSource. Cleaned up by key only.
    enteredWith :: Map.Map ObjectId.ObjectId (Set.Set EntryLink.EntryLink),
    -- | CR 400.7 / 607.1: the sources whose effects put counters on or removed
    -- counters from each object, as that object, keyed by the incarnation and
    -- written by Parun.Engine.Resolve.Effect.applyEffectWith off the
    -- instruction's counter events, never a case over the opcode. Read by
    -- Filter.CounterLinkedToSource. Cleaned up by key only.
    counteredWith :: Map.Map ObjectId.ObjectId (Set.Set CounterLink.CounterLink),
    -- | CR 406.4: which pile each face-down exiled card is in, a name drawn from
    -- nextTimestamp and shared by every card one execution of one instruction
    -- exiled (Parun.Engine.Resolve.Common.recordExilePile). Cleaned up by key only.
    exilePiles :: Map.Map ObjectId.ObjectId Timestamp.Timestamp,
    -- | CR 607.2n: the name of the cards each card exiled before the game was
    -- exiled with, keyed by the exiled incarnation and written by
    -- Parun.Engine.Setup.createDeck; its owner is the player who exiled it (CR
    -- 108.3). Cleaned up by key only.
    exiledBeforeGame :: Map.Map ObjectId.ObjectId CardName.CardName,
    -- | CR 500.7: the extra turns created and not yet taken, most recently
    -- created first, each carrying the steps that turn skips (CR 500.11).
    extraTurns :: [ExtraTurn.ExtraTurn],
    -- | CR 807.4: a Grand Melee game's turn markers in number order, written
    -- by Parun.Engine.GrandMelee; empty in every other game.
    turnMarkers :: [TurnMarker.TurnMarker],
    -- | CR 807.4b / 807.5: every running Grand Melee turn but `turn`, by its
    -- marker's number (Parun.Engine.Engine.runStep's scheduler); empty in every
    -- other game.
    parkedTurns :: Map.Map Natural.Natural TurnRegisters.TurnRegisters
  }
  deriving (Eq, Ord, Show)
