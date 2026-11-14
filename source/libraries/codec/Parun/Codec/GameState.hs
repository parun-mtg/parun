{-# LANGUAGE ApplicativeDo #-}

-- | A game in progress, on the wire (#126).
module Parun.Codec.GameState where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Codec.AbilityTriggered as AbilityTriggered
import qualified Parun.Codec.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Parun.Codec.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Parun.Codec.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Parun.Codec.ActiveCopy as ActiveCopy
import qualified Parun.Codec.ActiveEvasion as ActiveEvasion
import qualified Parun.Codec.ActiveLaterPayment as ActiveLaterPayment
import qualified Parun.Codec.ActiveObjectProhibition as ActiveObjectProhibition
import qualified Parun.Codec.ActivePlayerEffect as ActivePlayerEffect
import qualified Parun.Codec.ActiveReplacement as ActiveReplacement
import qualified Parun.Codec.Arrival as Arrival
import qualified Parun.Codec.BattlefieldCandidate as BattlefieldCandidate
import qualified Parun.Codec.Binding as Binding
import qualified Parun.Codec.Card as Card
import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.Cause as Cause
import qualified Parun.Codec.ContinuousEffect as ContinuousEffect
import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.CounterLink as CounterLink
import qualified Parun.Codec.Daytime as Daytime
import qualified Parun.Codec.DelayedTrigger as DelayedTrigger
import qualified Parun.Codec.EnteringTogether as EnteringTogether
import qualified Parun.Codec.EntryLink as EntryLink
import qualified Parun.Codec.EntryOrigin as EntryOrigin
import qualified Parun.Codec.EventGroup as EventGroup
import qualified Parun.Codec.ExileLink as ExileLink
import qualified Parun.Codec.ExtraTurn as ExtraTurn
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.GameSettings as GameSettings
import qualified Parun.Codec.IgnoredAbility as IgnoredAbility
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.LastKnown as LastKnown
import qualified Parun.Codec.Mana as Mana
import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.Codec.Object as Object
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.OutsideCard as OutsideCard
import qualified Parun.Codec.OutsideObject as OutsideObject
import qualified Parun.Codec.PastTurn as PastTurn
import qualified Parun.Codec.PendingEntryEffect as PendingEntryEffect
import qualified Parun.Codec.PendingReplacementEffect as PendingReplacementEffect
import qualified Parun.Codec.PendingSkipEffect as PendingSkipEffect
import qualified Parun.Codec.PhasedOut as PhasedOut
import qualified Parun.Codec.Player as Player
import qualified Parun.Codec.PlayerControl as PlayerControl
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Prevention as Prevention
import qualified Parun.Codec.Printing as Printing
import qualified Parun.Codec.PrintingId as PrintingId
import qualified Parun.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Codec.RepeatPicks as RepeatPicks
import qualified Parun.Codec.RestartSignal as RestartSignal
import qualified Parun.Codec.Result as Result
import qualified Parun.Codec.ReturnWatch as ReturnWatch
import qualified Parun.Codec.SideDeck as SideDeck
import qualified Parun.Codec.ThrowResult as ThrowResult
import qualified Parun.Codec.Timestamp as Timestamp
import qualified Parun.Codec.TurnMarker as TurnMarker
import qualified Parun.Codec.TurnRegisters as TurnRegisters
import qualified Parun.Codec.Undoing as Undoing
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Bookkeeping as Bookkeeping
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.History as History
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.Printing as Printing.Type
import qualified Parun.Types.PrintingId as PrintingId.Type
import qualified Parun.Types.RestartSignal as RestartSignal.Type
import qualified Parun.Types.Scratch as Scratch
import qualified Parun.Types.SlotName as SlotName.Type
import qualified Parun.Types.Snapshots as Snapshots
import qualified Parun.Types.StoredEffects as StoredEffects

-- | Every field but one, in the record's own order.
--
-- The fields of GameState's groups -- `match`, `stored`, `bookkeeping`,
-- `snapshots`, `scratch` and `history` -- are written FLAT, each under its own
-- key at the place it held before the grouping, rather than nested under a key
-- of the group's: the grouping is the engine's, for Parun.Engine.Stored, the CR
-- 603.12 comparison and Parun.Engine.Setup's restart and subgame, and the wire
-- has no reader of it.
--
-- 'Fields.required' is kept for the fields no game can be missing -- `settings`,
-- `objects`, `players`, `seating`, `turn` -- and for the counters and clocks
-- whose zero is a position rather than an absence: `nextEventGroup`,
-- `nextObjectId`, `nextPrintingId`, `nextTimestamp` and `lastChoice`.
-- Parun.Codec.TurnRegisters keeps the same split for `turn`'s own fields.
--
-- Everything else is 'Fields.defaulted', which omits a field equal to its
-- default and reads an absence back as that same value, so the round trip is
-- unchanged and a state that has done nothing writes almost nothing. The trap
-- it introduces is that a mistyped key now decodes as the default instead of
-- failing as a missing one; rejecting unknown keys is not the fix, since
-- 'Parun.JsonCodec.Arm.armObject' already declines @additionalProperties@ for
-- forward compatibility.
--
-- The resolver goes to `printings` and nowhere else: Parun.Codec.Printing's
-- 'Printing.reference' writes a card's NAME where the resolver reproduces it and
-- the whole record where none can. It is the caller's, because @parun:registry@
-- sits above @parun:codec@ and a decoder cannot reach one.
--
-- Bookkeeping.printingIds is NOT written. It is `printings` read backwards, which
-- Parun.Types.GameState says at the field itself, so writing it would put the
-- same table on the wire twice and let the two disagree -- a document whose two
-- halves contradict each other has no meaning this decoder could pick. It is
-- rebuilt by `invert` below instead.
--
-- Its key is also a whole Printing, so it could not be an object key at all: it
-- would need an array of key/value pairs, duplicating every printing's entire
-- record a second time. That is the size argument, and it is the lesser one.
codec :: (CardName.Type.CardName -> Maybe Card.Type.Card) -> Codec.Codec GameState.GameState
codec resolve = Fields.object $ do
  settings <- Fields.required "settings" GameSettings.codec (MatchScope.settings . GameState.match)
  objects <- Fields.required "objects" (Common.naturalMap ObjectId.codec Object.codec) GameState.objects
  library <- Fields.defaulted "library" Map.empty (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.library
  hand <- Fields.defaulted "hand" Map.empty (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.hand
  graveyard <- Fields.defaulted "graveyard" Map.empty (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)) GameState.graveyard
  battlefield <- Fields.defaulted "battlefield" Set.empty (Common.set ObjectId.codec) GameState.battlefield
  phasedOut <- Fields.defaulted "phasedOut" Map.empty (Common.naturalMap ObjectId.codec PhasedOut.codec) GameState.phasedOut
  exile <- Fields.defaulted "exile" Set.empty (Common.set ObjectId.codec) GameState.exile
  command <- Fields.defaulted "command" Set.empty (Common.set ObjectId.codec) GameState.command
  ante <- Fields.defaulted "ante" Set.empty (Common.set ObjectId.codec) GameState.ante
  sideDecks <- Fields.defaulted "sideDecks" Map.empty (Common.keyedList (Common.keyValue SideDeck.codec (Common.naturalMap PlayerId.codec (Common.seq ObjectId.codec)))) GameState.sideDecks
  turn <- Fields.required "turn" TurnRegisters.codec GameState.turn
  players <- Fields.required "players" (Common.naturalMap PlayerId.codec Player.codec) GameState.players
  outsideObjects <- Fields.defaulted "outsideObjects" Map.empty (Common.naturalMap ObjectId.codec OutsideObject.codec) GameState.outsideObjects
  broughtIn <- Fields.defaulted "broughtIn" Seq.empty (Common.seq ObjectId.codec) GameState.broughtIn
  departedCards <- Fields.defaulted "departedCards" Map.empty (Common.naturalMap ObjectId.codec Object.codec) GameState.departedCards
  manaPool <- Fields.defaulted "manaPool" Map.empty (Common.naturalMap PlayerId.codec Mana.codec) GameState.manaPool
  nextEventGroup <- Fields.required "nextEventGroup" EventGroup.codec (Bookkeeping.nextEventGroup . GameState.bookkeeping)
  eventGroupDepth <- Fields.defaulted "eventGroupDepth" 0 Common.natural (Bookkeeping.eventGroupDepth . GameState.bookkeeping)
  enteringTogether <- Fields.defaulted "enteringTogether" Nothing (Common.maybe EnteringTogether.codec) (Scratch.enteringTogether . GameState.scratch)
  arrivals <- Fields.defaulted "arrivals" Nothing (Common.maybe (Common.seq Arrival.codec)) (Scratch.arrivals . GameState.scratch)
  payingOnResolution <- Fields.defaulted "payingOnResolution" False Common.boolean (Scratch.payingOnResolution . GameState.scratch)
  causing <- Fields.defaulted "causing" Nothing (Common.maybe Cause.codec) (Scratch.causing . GameState.scratch)
  lastKnown <- Fields.defaulted "lastKnown" Map.empty (Common.naturalMap ObjectId.codec LastKnown.codec) (Snapshots.lastKnown . GameState.snapshots)
  stackArchive <- Fields.defaulted "stackArchive" Map.empty (Common.naturalMap ObjectId.codec Object.codec) (Snapshots.stackArchive . GameState.snapshots)
  battlefieldWhenTriggered <- Fields.defaulted "battlefieldWhenTriggered" Map.empty (Common.naturalMap EventGroup.codec (Common.naturalMap ObjectId.codec (BattlefieldCandidate.codec ProjectedCharacteristics.codec))) (Snapshots.battlefieldWhenTriggered . GameState.snapshots)
  boardAfterEvent <- Fields.defaulted "boardAfterEvent" Map.empty (Common.naturalMap EventGroup.codec (codec resolve)) (Snapshots.boardAfterEvent . GameState.snapshots)
  boardBeforeEvent <- Fields.defaulted "boardBeforeEvent" Map.empty (Common.naturalMap EventGroup.codec (codec resolve)) (Snapshots.boardBeforeEvent . GameState.snapshots)
  controlSample <- Fields.defaulted "controlSample" Map.empty (Common.naturalMap ObjectId.codec PlayerId.codec) (Snapshots.controlSample . GameState.snapshots)
  boardAtControlSample <- Fields.defaulted "boardAtControlSample" Nothing (Common.maybe (codec resolve)) (Snapshots.boardAtControlSample . GameState.snapshots)
  heldBoards <- Fields.defaulted "heldBoards" [] (Common.list (codec resolve)) (Snapshots.heldBoards . GameState.snapshots)
  delayedTriggers <- Fields.defaulted "delayedTriggers" Seq.empty (Common.seq DelayedTrigger.codec) (StoredEffects.delayedTriggers . GameState.stored)
  continuousEffects <- Fields.defaulted "continuousEffects" [] (Common.list (ContinuousEffect.codec Card.codec)) (StoredEffects.continuousEffects . GameState.stored)
  copyEffects <- Fields.defaulted "copyEffects" [] (Common.list ActiveCopy.codec) (StoredEffects.copyEffects . GameState.stored)
  replacements <- Fields.defaulted "replacements" [] (Common.list ActiveReplacement.codec) (StoredEffects.replacements . GameState.stored)
  laterPayments <- Fields.defaulted "laterPayments" [] (Common.list ActiveLaterPayment.codec) (StoredEffects.laterPayments . GameState.stored)
  pendingPreventionRiders <- Fields.defaulted "pendingPreventionRiders" Seq.empty (Common.seq Prevention.codec) (StoredEffects.pendingPreventionRiders . GameState.stored)
  pendingReplacementEffects <- Fields.defaulted "pendingReplacementEffects" Seq.empty (Common.seq PendingReplacementEffect.codec) (StoredEffects.pendingReplacementEffects . GameState.stored)
  pendingSkipEffects <- Fields.defaulted "pendingSkipEffects" Seq.empty (Common.seq PendingSkipEffect.codec) (StoredEffects.pendingSkipEffects . GameState.stored)
  contested <- Fields.defaulted "contested" Map.empty (Common.naturalMap ObjectId.codec (Common.maybe ObjectId.codec)) (Scratch.contested . GameState.scratch)
  ambientAmounts <- Fields.defaulted "ambientAmounts" Map.empty (Common.textMap SlotName.Type.unwrap (Right . SlotName.Type.MkSlotName) Common.natural) (Scratch.ambientAmounts . GameState.scratch)
  detachedBindings <- Fields.defaulted "detachedBindings" Map.empty (Common.naturalMap ObjectId.codec Binding.codecMap) GameState.detachedBindings
  pendingEntryEffects <- Fields.defaulted "pendingEntryEffects" Seq.empty (Common.seq PendingEntryEffect.codec) (StoredEffects.pendingEntryEffects . GameState.stored)
  enteringBeside <- Fields.defaulted "enteringBeside" Set.empty (Common.set ObjectId.codec) (Scratch.enteringBeside . GameState.scratch)
  enteringSubjects <- Fields.defaulted "enteringSubjects" Map.empty (Common.naturalMap ObjectId.codec (Common.maybe EntryOrigin.codec)) (Scratch.enteringSubjects . GameState.scratch)
  enteredFrom <- Fields.defaulted "enteredFrom" Map.empty (Common.naturalMap ObjectId.codec EntryOrigin.codec) (Scratch.enteredFrom . GameState.scratch)
  enteringPending <- Fields.defaulted "enteringPending" Map.empty (Common.naturalMap ObjectId.codec PlayerId.codec) (Scratch.enteringPending . GameState.scratch)
  refusedEntries <- Fields.defaulted "refusedEntries" Nothing (Common.maybe (Common.set ObjectId.codec)) (Scratch.refusedEntries . GameState.scratch)
  enteringCounters <- Fields.defaulted "enteringCounters" Map.empty (Common.naturalMap ObjectId.codec (Common.keyedList (Common.keyValue (Common.maybe PlayerId.codec) (Common.multiset (CounterKind.codec Keyword.codec))))) (Scratch.enteringCounters . GameState.scratch)
  copyExceptionCounters <- Fields.defaulted "copyExceptionCounters" Map.empty (Common.naturalMap ObjectId.codec (Common.multiset (CounterKind.codec Keyword.codec))) (Scratch.copyExceptionCounters . GameState.scratch)
  playerEffects <- Fields.defaulted "playerEffects" [] (Common.list ActivePlayerEffect.codec) (StoredEffects.playerEffects . GameState.stored)
  blockRequirements <- Fields.defaulted "blockRequirements" [] (Common.list ActiveBlockRequirement.codec) (StoredEffects.blockRequirements . GameState.stored)
  attackRequirements <- Fields.defaulted "attackRequirements" [] (Common.list ActiveAttackRequirement.codec) (StoredEffects.attackRequirements . GameState.stored)
  objectProhibitions <- Fields.defaulted "objectProhibitions" [] (Common.list ActiveObjectProhibition.codec) (StoredEffects.objectProhibitions . GameState.stored)
  attackProhibitions <- Fields.defaulted "attackProhibitions" [] (Common.list ActiveAttackProhibition.codec) (StoredEffects.attackProhibitions . GameState.stored)
  evasions <- Fields.defaulted "evasions" [] (Common.list ActiveEvasion.codec) (StoredEffects.evasions . GameState.stored)
  ignoredAbilities <- Fields.defaulted "ignoredAbilities" [] (Common.list IgnoredAbility.codec) (StoredEffects.ignoredAbilities . GameState.stored)
  seating <- Fields.required "seating" (Common.list PlayerId.codec) GameState.seating
  turnOrderReversed <- Fields.defaulted "turnOrderReversed" False Common.boolean GameState.turnOrderReversed
  turnsBegun <- Fields.defaulted "turnsBegun" Map.empty (Common.naturalMap PlayerId.codec Common.natural) GameState.turnsBegun
  result <- Fields.defaulted "result" Nothing (Common.maybe Result.codec) GameState.result
  restartSignal <- Fields.defaulted "restartSignal" RestartSignal.Type.Playing RestartSignal.codec GameState.restartSignal
  nextObjectId <- Fields.required "nextObjectId" ObjectId.codec (Bookkeeping.nextObjectId . GameState.bookkeeping)
  printings <- Fields.defaulted "printings" Map.empty (Common.naturalMap PrintingId.codec (Printing.reference resolve)) (Bookkeeping.printings . GameState.bookkeeping)
  lookedUp <- Fields.defaulted "lookedUp" Set.empty (Common.set PrintingId.codec) (Bookkeeping.lookedUp . GameState.bookkeeping)
  referenceNames <- Fields.defaulted "referenceNames" Map.empty (Common.keyedList (Common.keyValue (Filter.codec Keyword.codec) (Common.set CardName.codec))) (Bookkeeping.referenceNames . GameState.bookkeeping)
  outsideCopies <- Fields.defaulted "outsideCopies" Set.empty (Common.set ObjectId.codec) GameState.outsideCopies
  outsideFinds <- Fields.defaulted "outsideFinds" Map.empty (Common.naturalMap ObjectId.codec OutsideCard.codec) GameState.outsideFinds
  namedCopyChoices <- Fields.defaulted "namedCopyChoices" Map.empty (Common.naturalMap ObjectId.codec (Common.set CardName.codec)) GameState.namedCopyChoices
  notedCards <- Fields.defaulted "notedCards" Map.empty (Common.naturalMap ObjectId.codec PrintingId.codec) GameState.notedCards
  notedMana <- Fields.defaulted "notedMana" Map.empty (Common.naturalMap ObjectId.codec (Common.seq ManaType.codec)) GameState.notedMana
  nextPrintingId <- Fields.required "nextPrintingId" PrintingId.codec (Bookkeeping.nextPrintingId . GameState.bookkeeping)
  nextTimestamp <- Fields.required "nextTimestamp" Timestamp.codec (Bookkeeping.nextTimestamp . GameState.bookkeeping)
  lastChoice <- Fields.required "lastChoice" Timestamp.codec (Bookkeeping.lastChoice . GameState.bookkeeping)
  loopInvolvement <- Fields.defaulted "loopInvolvement" Map.empty (Common.naturalMap PlayerId.codec Timestamp.codec) GameState.loopInvolvement
  drewFromEmpty <- Fields.defaulted "drewFromEmpty" Set.empty (Common.set PlayerId.codec) GameState.drewFromEmpty
  fellShort <- Fields.defaulted "fellShort" 0 Common.natural (Bookkeeping.fellShort . GameState.bookkeeping)
  landsPlayed <- Fields.defaulted "landsPlayed" Map.empty (Common.naturalMap PlayerId.codec Common.natural) GameState.landsPlayed
  cardsPlayed <- Fields.defaulted "cardsPlayed" Map.empty (Common.naturalMap ObjectId.codec PlayerId.codec) GameState.cardsPlayed
  keptFaceDown <- Fields.defaulted "keptFaceDown" Nothing (Common.maybe (Common.naturalMap ObjectId.codec Common.natural)) (Scratch.keptFaceDown . GameState.scratch)
  announcedTops <- Fields.defaulted "announcedTops" Nothing (Common.maybe (Common.naturalMap PlayerId.codec ObjectId.codec)) (Scratch.announcedTops . GameState.scratch)
  actionTops <- Fields.defaulted "actionTops" Nothing (Common.maybe (Common.naturalMap PlayerId.codec ObjectId.codec)) (Scratch.actionTops . GameState.scratch)
  loopPicks <- Fields.defaulted "loopPicks" Nothing (Common.maybe (Common.seq (Common.tuple PlayerId.codec (Common.set ObjectId.codec)))) (Scratch.loopPicks . GameState.scratch)
  repeatPicks <- Fields.defaulted "repeatPicks" Nothing (Common.maybe RepeatPicks.codec) (Bookkeeping.repeatPicks . GameState.bookkeeping)
  revealedTops <- Fields.defaulted "revealedTops" Map.empty (Common.naturalMap PlayerId.codec ObjectId.codec) GameState.revealedTops
  departedThisTurn <- Fields.defaulted "departedThisTurn" Set.empty (Common.set PlayerId.codec) GameState.departedThisTurn
  -- Common.repeats, for Parun.Codec.Object's activatedOnce reason.
  triggeredThisGame <- Fields.defaulted "triggeredThisGame" Seq.empty (Common.seq AbilityTriggered.codec) (History.triggeredThisGame . GameState.history)
  pendingControl <- Fields.defaulted "pendingControl" Map.empty (Common.naturalMap PlayerId.codec (Common.nonEmpty PlayerControl.codec)) GameState.pendingControl
  control <- Fields.defaulted "control" Map.empty (Common.naturalMap PlayerId.codec (Common.nonEmpty PlayerControl.codec)) GameState.control
  monarch <- Fields.defaulted "monarch" Nothing (Common.maybe PlayerId.codec) GameState.monarch
  initiative <- Fields.defaulted "initiative" Nothing (Common.maybe PlayerId.codec) GameState.initiative
  daytime <- Fields.defaulted "daytime" Nothing (Common.maybe Daytime.codec) GameState.daytime
  pastTurns <- Fields.defaulted "pastTurns" Seq.empty (Common.seq PastTurn.codec) (History.pastTurns . GameState.history)
  damagePreventedBy <- Fields.defaulted "damagePreventedBy" Map.empty (Common.naturalMap ObjectId.codec Common.natural) GameState.damagePreventedBy
  expiredLastTurns <- Fields.defaulted "expiredLastTurns" Set.empty (Common.set PlayerId.codec) (History.expiredLastTurns . GameState.history)
  resolvedNames <- Fields.defaulted "resolvedNames" Map.empty (Common.naturalMap PlayerId.codec (Common.set CardName.codec)) (History.resolvedNames . GameState.history)
  movedUntil <- Fields.defaulted "movedUntil" Map.empty (Common.naturalMap ObjectId.codec ReturnWatch.codec) GameState.movedUntil
  haunting <- Fields.defaulted "haunting" Map.empty (Common.naturalMap ObjectId.codec ObjectId.codec) GameState.haunting
  encoded <- Fields.defaulted "encoded" Map.empty (Common.naturalMap ObjectId.codec ObjectId.codec) GameState.encoded
  exiledWith <- Fields.defaulted "exiledWith" Map.empty (Common.naturalMap ObjectId.codec ExileLink.codec) GameState.exiledWith
  enteredWith <- Fields.defaulted "enteredWith" Map.empty (Common.naturalMap ObjectId.codec (Common.set EntryLink.codec)) GameState.enteredWith
  counteredWith <- Fields.defaulted "counteredWith" Map.empty (Common.naturalMap ObjectId.codec (Common.set CounterLink.codec)) GameState.counteredWith
  exilePiles <- Fields.defaulted "exilePiles" Map.empty (Common.naturalMap ObjectId.codec Timestamp.codec) GameState.exilePiles
  exiledBeforeGame <- Fields.defaulted "exiledBeforeGame" Map.empty (Common.naturalMap ObjectId.codec CardName.codec) GameState.exiledBeforeGame
  extraTurns <- Fields.defaulted "extraTurns" [] (Common.list ExtraTurn.codec) GameState.extraTurns
  turnMarkers <- Fields.defaulted "turnMarkers" [] (Common.list TurnMarker.codec) GameState.turnMarkers
  parkedTurns <- Fields.defaulted "parkedTurns" Map.empty (Common.naturalMap Common.natural TurnRegisters.codec) GameState.parkedTurns
  subgamesThisMatch <- Fields.defaulted "subgamesThisMatch" 0 Common.natural (MatchScope.subgamesThisMatch . GameState.match)
  gamesThisMatch <- Fields.defaulted "gamesThisMatch" 1 Common.natural (MatchScope.gamesThisMatch . GameState.match)
  gameNumber <- Fields.defaulted "gameNumber" 1 Common.natural (MatchScope.gameNumber . GameState.match)
  throwing <- Fields.defaulted "throwing" Nothing (Common.maybe ThrowResult.codec) (Scratch.throwing . GameState.scratch)
  rethrowRequested <- Fields.defaulted "rethrowRequested" False Common.boolean (Scratch.rethrowRequested . GameState.scratch)
  undone <- Fields.defaulted "undone" Map.empty (Common.naturalMap PlayerId.codec Undoing.codec) (Bookkeeping.undone . GameState.bookkeeping)
  pure
    GameState.MkGameState
      { GameState.match =
          MatchScope.MkMatchScope
            { MatchScope.settings = settings,
              MatchScope.subgamesThisMatch = subgamesThisMatch,
              MatchScope.gamesThisMatch = gamesThisMatch,
              MatchScope.gameNumber = gameNumber
            },
        GameState.objects = objects,
        GameState.library = library,
        GameState.hand = hand,
        GameState.graveyard = graveyard,
        GameState.battlefield = battlefield,
        GameState.phasedOut = phasedOut,
        GameState.exile = exile,
        GameState.command = command,
        GameState.ante = ante,
        GameState.sideDecks = sideDecks,
        GameState.turn = turn,
        GameState.players = players,
        GameState.outsideObjects = outsideObjects,
        GameState.broughtIn = broughtIn,
        GameState.departedCards = departedCards,
        GameState.manaPool = manaPool,
        GameState.scratch =
          Scratch.MkScratch
            { Scratch.enteringTogether = enteringTogether,
              Scratch.arrivals = arrivals,
              Scratch.payingOnResolution = payingOnResolution,
              Scratch.causing = causing,
              Scratch.contested = contested,
              Scratch.ambientAmounts = ambientAmounts,
              Scratch.enteringBeside = enteringBeside,
              Scratch.enteringSubjects = enteringSubjects,
              Scratch.enteredFrom = enteredFrom,
              Scratch.enteringPending = enteringPending,
              Scratch.refusedEntries = refusedEntries,
              Scratch.enteringCounters = enteringCounters,
              Scratch.copyExceptionCounters = copyExceptionCounters,
              Scratch.keptFaceDown = keptFaceDown,
              Scratch.announcedTops = announcedTops,
              Scratch.actionTops = actionTops,
              Scratch.loopPicks = loopPicks,
              Scratch.throwing = throwing,
              Scratch.rethrowRequested = rethrowRequested
            },
        GameState.bookkeeping =
          Bookkeeping.MkBookkeeping
            { Bookkeeping.nextObjectId = nextObjectId,
              Bookkeeping.nextPrintingId = nextPrintingId,
              Bookkeeping.nextTimestamp = nextTimestamp,
              Bookkeeping.nextEventGroup = nextEventGroup,
              Bookkeeping.eventGroupDepth = eventGroupDepth,
              Bookkeeping.printings = printings,
              -- Derived rather than written: see the note above.
              Bookkeeping.printingIds = invert printings,
              Bookkeeping.lookedUp = lookedUp,
              Bookkeeping.referenceNames = referenceNames,
              Bookkeeping.lastChoice = lastChoice,
              Bookkeeping.repeatPicks = repeatPicks,
              Bookkeeping.undone = undone,
              Bookkeeping.fellShort = fellShort
            },
        GameState.snapshots =
          Snapshots.MkSnapshots
            { Snapshots.lastKnown = lastKnown,
              Snapshots.stackArchive = stackArchive,
              Snapshots.battlefieldWhenTriggered = battlefieldWhenTriggered,
              Snapshots.boardAfterEvent = boardAfterEvent,
              Snapshots.boardBeforeEvent = boardBeforeEvent,
              Snapshots.controlSample = controlSample,
              Snapshots.boardAtControlSample = boardAtControlSample,
              Snapshots.heldBoards = heldBoards
            },
        GameState.stored =
          StoredEffects.MkStoredEffects
            { StoredEffects.continuousEffects = continuousEffects,
              StoredEffects.copyEffects = copyEffects,
              StoredEffects.replacements = replacements,
              StoredEffects.playerEffects = playerEffects,
              StoredEffects.blockRequirements = blockRequirements,
              StoredEffects.attackRequirements = attackRequirements,
              StoredEffects.objectProhibitions = objectProhibitions,
              StoredEffects.attackProhibitions = attackProhibitions,
              StoredEffects.evasions = evasions,
              StoredEffects.ignoredAbilities = ignoredAbilities,
              StoredEffects.laterPayments = laterPayments,
              StoredEffects.delayedTriggers = delayedTriggers,
              StoredEffects.pendingPreventionRiders = pendingPreventionRiders,
              StoredEffects.pendingReplacementEffects = pendingReplacementEffects,
              StoredEffects.pendingSkipEffects = pendingSkipEffects,
              StoredEffects.pendingEntryEffects = pendingEntryEffects
            },
        GameState.detachedBindings = detachedBindings,
        GameState.seating = seating,
        GameState.turnOrderReversed = turnOrderReversed,
        GameState.turnsBegun = turnsBegun,
        GameState.result = result,
        GameState.restartSignal = restartSignal,
        GameState.outsideCopies = outsideCopies,
        GameState.outsideFinds = outsideFinds,
        GameState.namedCopyChoices = namedCopyChoices,
        GameState.notedCards = notedCards,
        GameState.notedMana = notedMana,
        GameState.loopInvolvement = loopInvolvement,
        GameState.drewFromEmpty = drewFromEmpty,
        GameState.landsPlayed = landsPlayed,
        GameState.cardsPlayed = cardsPlayed,
        GameState.revealedTops = revealedTops,
        GameState.departedThisTurn = departedThisTurn,
        GameState.history =
          History.MkHistory
            { History.triggeredThisGame = triggeredThisGame,
              History.pastTurns = pastTurns,
              History.expiredLastTurns = expiredLastTurns,
              History.resolvedNames = resolvedNames
            },
        GameState.pendingControl = pendingControl,
        GameState.control = control,
        GameState.monarch = monarch,
        GameState.initiative = initiative,
        GameState.daytime = daytime,
        GameState.damagePreventedBy = damagePreventedBy,
        GameState.movedUntil = movedUntil,
        GameState.haunting = haunting,
        GameState.encoded = encoded,
        GameState.exiledWith = exiledWith,
        GameState.enteredWith = enteredWith,
        GameState.counteredWith = counteredWith,
        GameState.exilePiles = exilePiles,
        GameState.exiledBeforeGame = exiledBeforeGame,
        GameState.extraTurns = extraTurns,
        GameState.turnMarkers = turnMarkers,
        GameState.parkedTurns = parkedTurns
      }

-- | `printings` read backwards, which is what Bookkeeping.printingIds holds.
--
-- Exact, because Parun.Engine.Game.intern is idempotent: a printing already in
-- the table answers with the id it already has, so no two ids name one printing
-- and the inversion loses nothing. That idempotence is asserted by Parun.GameSpec's
-- "intern is idempotent for a printing already in the table", and the inversion
-- itself by Parun.CodecIntegrationSpec's "the intern table survives, though only
-- half of it is written".
invert :: Map.Map PrintingId.Type.PrintingId Printing.Type.Printing -> Map.Map Printing.Type.Printing PrintingId.Type.PrintingId
invert = Map.fromList . fmap (\(i, p) -> (p, i)) . Map.toList
