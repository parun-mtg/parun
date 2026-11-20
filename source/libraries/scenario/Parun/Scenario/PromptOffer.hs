{-# LANGUAGE GADTs #-}

-- | What a prompt offers, in the one spelling both the line protocol and a
-- scenario's Prompt view read: the line protocol names objects by id, a
-- scenario by label, each through its own 'Reply.Namer'.
module Parun.Scenario.PromptOffer where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.AbilityName as Codec.AbilityName
import qualified Parun.Codec.CardName as Codec.CardName
import qualified Parun.Codec.CombatDamageOption as Codec.CombatDamageOption
import qualified Parun.Codec.Cost as Codec.Cost
import qualified Parun.Codec.CostComponent as Codec.CostComponent
import qualified Parun.Codec.EntryOption as Codec.EntryOption
import qualified Parun.Codec.Filter as Codec.Filter
import qualified Parun.Codec.GameEvent as Codec.GameEvent
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.LibraryPosition as Codec.LibraryPosition
import qualified Parun.Codec.ManaCost as Codec.ManaCost
import qualified Parun.Codec.ManaSymbol as Codec.ManaSymbol
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.ManaUnit as Codec.ManaUnit
import qualified Parun.Codec.NumberFloor as Codec.NumberFloor
import qualified Parun.Codec.OptionalDecision as Codec.OptionalDecision
import qualified Parun.Codec.OutsideCard as Codec.OutsideCard
import qualified Parun.Codec.PlayerEffect as Codec.PlayerEffect
import qualified Parun.Codec.Reference as Codec.Reference
import qualified Parun.Codec.SlotName as Codec.SlotName
import qualified Parun.Codec.WordFamily as Codec.WordFamily
import qualified Parun.Codec.Zone as Codec.Zone
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Keyword as Keyword
import qualified Parun.Engine.Mana as Mana
import qualified Parun.Engine.Modal as Modal
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Engine.Resolve as Resolve
import qualified Parun.Engine.Subtype as Subtype
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Oracle as Oracle
import qualified Parun.Oracle.Continuous as Oracle.Continuous
import qualified Parun.Oracle.Effect as Oracle.Effect
import qualified Parun.Oracle.Target as Oracle.Target
import qualified Parun.Scenario.Offer as Offer
import qualified Parun.Scenario.Reply as Reply
import qualified Parun.Types.Action as Action
import qualified Parun.Types.BallotChoice as BallotChoice
import qualified Parun.Types.BuybackDecision as BuybackDecision
import qualified Parun.Types.CandidateCost as CandidateCost
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.CommandZoneDecision as CommandZoneDecision
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CounterSpread as CounterSpread
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.Dividing as Dividing
import qualified Parun.Types.EntwineDecision as EntwineDecision
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.Face as Face
import qualified Parun.Types.ForageMode as ForageMode
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.HandActionIndex as HandActionIndex
import qualified Parun.Types.Keyword as Keyword.Type
import qualified Parun.Types.LandOrNonland as LandOrNonland
import qualified Parun.Types.LastKnown as LastKnown
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.LoopMembers as LoopMembers
import qualified Parun.Types.Mana as Mana.Type
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaUnit as ManaUnit
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.MoveSpread as MoveSpread
import qualified Parun.Types.MulliganOffer as MulliganOffer
import qualified Parun.Types.MutateSide as MutateSide
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.OutsideCard as OutsideCard
import qualified Parun.Types.OutsideObject as OutsideObject
import qualified Parun.Types.Parity as Parity
import qualified Parun.Types.PayOffer as PayOffer
import qualified Parun.Types.PayerAnswer as PayerAnswer
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintedReplacement as PrintedReplacement
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.ReplacementEntry as ReplacementEntry
import qualified Parun.Types.Reply as R
import qualified Parun.Types.RollAdjustment as RollAdjustment
import qualified Parun.Types.SearchPlace as SearchPlace
import qualified Parun.Types.Snapshots as Snapshots
import qualified Parun.Types.SpellWasCast as SpellWasCast
import qualified Parun.Types.TargetCount as TargetCount
import qualified Parun.Types.ThrowResult as ThrowResult
import qualified Parun.Types.TriggerEntry as TriggerEntry
import qualified Parun.Types.TriggerSource as TriggerSource
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Visibility as Visibility.Type
import qualified Parun.Types.Zone as Zone

-- | What a prompt offers, in its answer's vocabulary, with what the game
-- tells the seat about each option.
--
-- Every prompt not listed here sends null: Concede, which the client answers
-- from its seat's standing choice, and the random prompts no seat answers,
-- which are never sent. RollDie is never sent either; a scenario reads it.
offerOf :: Reply.Namer -> GameState.GameState -> Prompt.Prompt r -> R.Reply
offerOf namer gs prompt = case prompt of
  Prompt.ChooseAction _ _ offered -> R.Array (fmap (Offer.offer namer gs) offered)
  Prompt.DeclareMulligan _ _ offer ->
    fields [("taken", natural (MulliganOffer.taken offer)), ("bottom", natural (MulliganOffer.bottomCount offer))]
  Prompt.Bottom _ _ from count -> fields [("from", objects namer from), ("count", natural count)]
  Prompt.ChooseDiscard _ _ from count earlier -> fields [("from", objects namer from), ("count", natural count), ("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.setOf Reply.object))) namer earlier)]
  -- CR 404.3: the batch, from the top down as it would land.
  Prompt.ArrangeGraveyardArrivals _ _ batch -> fields [("from", objects namer batch)]
  Prompt.DeclareAttackers _ _ attackers -> objects namer attackers
  Prompt.DeclareBlockers _ _ blockers attackers -> fields [("blockers", objects namer blockers), ("attackers", objects namer attackers)]
  Prompt.DividePiles _ _ creatures count -> fields [("from", objects namer creatures), ("count", natural count)]
  Prompt.ChooseTargets _ _ source slots ->
    fields
      [ ("source", object namer source),
        ("slots", Reply.encode (Reply.mapOf (Reply.viaCodec Codec.SlotName.codec) (Reply.pairOf Reply.natural (Reply.setOf Reply.recipient))) namer slots)
      ]
  Prompt.ChooseManaSource _ payer cost sources ->
    fields
      ( [("payer", Reply.encode Reply.player namer payer), ("cost", Reply.encode (Reply.viaCodec Codec.ManaCost.codec) namer cost), ("sources", manaSources namer gs sources)]
          <> [("substitutes", Reply.encode (Reply.list (Reply.viaCodec Codec.Keyword.codec)) namer keywords) | let keywords = substitutingKeywords gs payer cost, not (null keywords)]
      )
  Prompt.ChooseExtraManaSource _ _ sources -> fields [("sources", manaSources namer gs sources)]
  -- CR 734.1: the sources of the one activation asked about.
  Prompt.ReverseManaAbilities _ _ sources -> fields [("sources", objects namer (NonEmpty.toList sources))]
  -- CR 601.2b / 702.51a: each candidate's cost and, beside it, the keyword
  -- that offered it (CR 702.152a) or null; CR 101.4b: what earlier payers
  -- answered. CR 603.7g: the delayed ability each creates or null, where one
  -- does. CR 118.9: the object whose budgeted grant each spends or null, where
  -- one does.
  Prompt.ChooseCost _ _ source candidates earlier ->
    fields
      ( [ ("source", object namer source),
          ("costs", Reply.encode (Reply.list (Reply.viaCodec (Codec.Cost.codec Codec.Keyword.codec))) namer (fmap CandidateCost.cost candidates)),
          ("keywords", Reply.encode (Reply.list (Reply.maybeOf (Reply.viaCodec Codec.Keyword.codec))) namer (fmap CandidateCost.keyword candidates)),
          ("earlier", payerAnswers namer earlier)
        ]
          <> [("arming", Reply.encode (Reply.list (Reply.maybeOf (Reply.viaCodec Codec.AbilityName.codec))) namer (fmap CandidateCost.arms candidates)) | any (Maybe.isJust . CandidateCost.arms) candidates]
          <> [("spending", Reply.encode (Reply.list (Reply.maybeOf Reply.object)) namer (fmap (fmap fst . CandidateCost.spends) candidates)) | any (Maybe.isJust . CandidateCost.spends) candidates]
      )
  Prompt.PayLifeForMana _ _ life mana -> fields [("life", natural life), ("mana", Reply.encode (Reply.viaCodec Codec.ManaType.codec) namer mana)]
  Prompt.ChooseManaYield _ _ source options -> fields [("source", object namer source), ("yields", Reply.encode (Reply.list (Reply.list (Reply.viaCodec Codec.ManaType.codec))) namer (fmap (fmap ManaUnit.manaType . Mana.yieldUnits) (Foldable.toList options)))]
  Prompt.ChooseManaSpentOnX _ _ source shares -> fields [("source", object namer source), ("shares", Reply.encode (Reply.list (Reply.list (Reply.viaCodec Codec.ManaType.codec))) namer (fmap (fmap ManaUnit.manaType . Mana.Type.unwrap) (Foldable.toList shares)))]
  Prompt.ChooseX _ _ source variable least greatest -> fields [("source", object namer source), ("variable", Reply.encode (Reply.viaCodec Codec.SlotName.codec) namer variable), ("least", natural least), ("greatest", natural greatest)]
  Prompt.ChooseNumber _ _ source least meaning most -> fields ([("source", object namer source), ("least", natural least), ("floor", Reply.encode (Reply.viaCodec Codec.NumberFloor.codec) namer meaning)] <> [("greatest", natural greatest) | Just greatest <- [most]])
  -- CR 601.2b / 702.33a: the keyword paid, from none to its limit where it has one.
  Prompt.ChooseKicker _ _ source keyword _ limit -> fields ([("source", object namer source), ("keyword", Reply.encode (Reply.viaCodec Codec.Keyword.codec) namer keyword), ("least", natural 0)] <> [("greatest", natural most) | Just most <- [limit]])
  -- CR 702.132a / 118.3: from none to what the assisting player's pool pays.
  Prompt.ChooseAssistAmount _ _ source most -> fields [("source", object namer source), ("least", natural 0), ("greatest", natural most)]
  -- CR 107.14 / 118.3: from none to the player's energy.
  Prompt.ChoosePaidEnergy _ _ source most -> fields [("source", object namer source), ("least", natural 0), ("greatest", natural most)]
  -- CR 715.3b: from one to the final chapter.
  Prompt.ChooseReadAheadChapter _ _ source final -> fields [("source", object namer source), ("least", natural 1), ("greatest", natural final)]
  -- CR 706.2b: the results in roll order and the amount, each die moved
  -- either way, then none.
  Prompt.AdjustDieRoll _ _ results amount cost ->
    choosing namer prompt "options" Nothing ([("results", R.Array (fmap R.Number (NonEmpty.toList results))), ("amount", natural amount)] <> [("cost", payment namer c) | Just c <- [cost]]) (fmap Just [(index, adjustment) | index <- zipWith const [0 ..] (NonEmpty.toList results), adjustment <- [RollAdjustment.Increase, RollAdjustment.Decrease]] <> [Nothing])
  -- CR 101.4b: what earlier seats answered to the same "may".
  Prompt.ChooseOptional _ _ source _ _ earlier -> fields [("source", object namer source), ("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.viaCodec Codec.OptionalDecision.codec))) namer earlier)]
  Prompt.ChooseGuess _ _ source proposition -> fields [("source", object namer source), ("proposition", Reply.encode (Reply.viaCodec (Codec.Filter.codec Codec.Keyword.codec)) namer proposition)]
  Prompt.AssignCombatDamage _ _ source lethal paid total ->
    fields [("source", object namer source), ("lethal", Reply.encode (Reply.mapOf Reply.recipient Reply.natural) namer lethal), ("paid", Reply.encode (Reply.mapOf Reply.recipient Reply.natural) namer paid), ("total", natural total)]
  Prompt.ChooseCombatDamageOption _ _ source option -> fields [("source", object namer source), ("option", Reply.encode (Reply.viaCodec Codec.CombatDamageOption.codec) namer option)]
  -- CR 118.12 / 101.4b: the cost, and what earlier payers answered; CR
  -- 608.2f: the member a loop's offer is for, where it is one.
  Prompt.ChooseToPay _ _ source offer cost earlier -> fields ([("source", object namer source), ("cost", payment namer cost), ("earlier", payerAnswers namer earlier)] <> [("member", Reply.encode Reply.recipient namer member) | PayOffer.ForMember member <- [offer]])
  Prompt.ChooseDivision _ _ source dividing targets ->
    fields
      ( [("source", object namer source), ("targets", Reply.encode (Reply.list Reply.recipient) namer (NonEmpty.toList targets))]
          <> case dividing of
            Dividing.Total total -> [("total", natural total)]
            Dividing.Parts parts -> [("parts", Reply.encode (Reply.list Reply.natural) namer (NonEmpty.toList parts))]
      )
  -- CR 615.7: each event's own amount, the points divided whole among them.
  Prompt.AllocateDamage _ _ events share -> amounts namer prompt Nothing [("entries", R.Array (fmap (damageEvent namer) events))] (Just share, Just share) (fmap DamageEvent.amount events)
  -- CR 601.2c: each slot's count up to what the board supplies, from its least
  -- where that is above none, beside its legal recipients.
  Prompt.AnnounceTargets _ _ source offers ->
    amounts
      namer
      prompt
      (Just source)
      ( [("recipients", Reply.encode (Reply.mapOf (Reply.viaCodec Codec.SlotName.codec) (Reply.setOf Reply.recipient)) namer (fmap snd offers))]
          <> [("floors", Reply.encode (Reply.shapeOf prompt) namer floors) | let floors = Map.filter (> 0) (fmap (TargetCount.least . fst) offers), not (Map.null floors)]
      )
      (Nothing, Nothing)
      (fmap (\(count, recipients) -> TargetCount.ceilingOn (Natural.length recipients) count) offers)
  -- CR 118.1 / 601.2h / 608.2d: what each permanent carries, exactly the
  -- count, at least it, or at most it.
  Prompt.ChooseCounterRemovalAmong _ _ source n carried -> amounts namer prompt (Just source) [] (Just n, Just n) carried
  Prompt.ChooseCounterRemovalAtLeast _ _ source n carried -> amounts namer prompt (Just source) [] (Just n, Nothing) carried
  Prompt.ChooseCounterRemovalUpTo _ _ source n carried -> amounts namer prompt (Just source) [] (Nothing, Just n) carried
  -- CR 122.1 / 601.2h: each kind each permanent carries, off one permanent
  -- where the cost says a permanent.
  Prompt.ChooseMixedCounterRemoval _ _ source spread n carried -> case spread of
    CounterSpread.FromOne -> amounts namer prompt (Just source) [("fromOne", R.Boolean True)] (Just n, Just n) carried
    CounterSpread.FromAmong -> amounts namer prompt (Just source) [] (Just n, Just n) carried
    CounterSpread.FromAmongAtLeast -> amounts namer prompt (Just source) [] (Just n, Nothing) carried
    CounterSpread.FromAmongAtMost -> amounts namer prompt (Just source) [] (Nothing, Just n) carried
  -- CR 122.5: the object the counters move off and onto, and each kind's tally,
  -- at least one in all where the card says one or more.
  Prompt.ChooseMovedCounters _ _ off onto kinds -> amounts namer prompt Nothing [("off", object namer off), ("onto", object namer onto)] (Nothing, Nothing) kinds
  Prompt.ChooseMovedCountersAtLeastOne _ _ off onto kinds -> amounts namer prompt Nothing [("off", object namer off), ("onto", object namer onto)] (Just 1, Nothing) kinds
  -- CR 122.5: each destination may take each kind's whole tally, and under
  -- @"counters"@ the tallies, which each kind's amounts in all stay within.
  Prompt.ChooseDistributedMovedCounters _ _ off spread kinds destinations ->
    let told = [("off", object namer off), ("counters", Reply.encode (Reply.mapOf Reply.counterKind Reply.natural) namer kinds)]
        whole = sum kinds
        caps = Map.fromList [(destination, kinds) | destination <- NonEmpty.toList destinations]
     in case spread of
          MoveSpread.AnyNumber -> amounts namer prompt Nothing told (Nothing, Nothing) caps
          MoveSpread.AtLeastOne -> amounts namer prompt Nothing told (Just 1, Nothing) caps
          MoveSpread.Exactly -> amounts namer prompt Nothing told (Just whole, Just whole) caps
          -- CR 609.3: that many of one kind, or its whole tally where fewer.
          MoveSpread.OneKind n -> amounts namer prompt Nothing (told <> [("oneKind", natural n)]) (Nothing, Just n) caps
          MoveSpread.UpToOne -> amounts namer prompt Nothing told (Nothing, Just 1) caps
  -- CR 608.2d: the whole count on any candidate, divided exactly.
  Prompt.ChooseDistribution _ _ source n candidates -> amounts namer prompt (Just source) [] (Just n, Just n) (Map.fromList [(candidate, n) | candidate <- NonEmpty.toList candidates])
  -- CR 706.8b: each stored result the object holds, by how many.
  Prompt.ChooseStoredRerolls _ _ source stored -> amounts namer prompt (Just source) [] (Nothing, Nothing) stored
  -- CR 119.7: each candidate beside its life total.
  Prompt.ChooseRedistribution _ _ totals -> fields [("totals", R.Array [R.Array [Reply.encode Reply.player namer pid, R.Number life] | (pid, life) <- totals])]
  -- CR 603.3b / 101.4c: each entry as its source and its Oracle text.
  Prompt.OrderTriggers _ _ entries -> R.Array (fmap (triggerEntry namer gs) entries)
  -- CR 601.2h: each pool unit whole, as the answer names it.
  Prompt.ChooseManaToSpend _ _ units -> Reply.encode (Reply.list (Reply.viaCodec Codec.ManaUnit.codec)) namer (NonEmpty.toList units)
  -- CR 701.23a: the searcher looks at the cards searched, so each is named.
  Prompt.Search _ _ cards count -> fields [("from", objects namer cards), ("names", R.Array (fmap (R.Text . snapshotName . (`Projection.project` gs)) cards)), ("count", natural count)]
  Prompt.ChooseConjuredCard _ _ names -> choosing namer prompt "options" Nothing [] (NonEmpty.toList names)
  -- CR 807.5b: the turn markers, by number, whose stacks the seat holds priority for.
  Prompt.ChooseTurnMarker _ _ markers -> choosing namer prompt "options" Nothing [] (NonEmpty.toList markers)
  Prompt.ChooseCoinResult _ _ faces -> choosing namer prompt "options" Nothing [] (NonEmpty.toList faces)
  Prompt.ChooseDefender _ _ players -> choosing namer prompt "players" Nothing [] (NonEmpty.toList players)
  Prompt.ChooseRingBearer _ _ candidates -> choosing namer prompt "options" Nothing [] (NonEmpty.toList candidates)
  Prompt.ChooseBolster _ _ source candidates -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList candidates)
  Prompt.ChooseAmass _ _ source candidates -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList candidates)
  Prompt.ChooseBlight _ _ source candidates -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList candidates)
  Prompt.ChooseBehold _ _ source candidates -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList candidates)
  -- Every choice of one vote is of one kind (Parun.Types.VoteChoices); then
  -- no ballot where it is offered.
  Prompt.ChooseVote _ _ source choices abstain -> choosing namer prompt (case NonEmpty.head choices of BallotChoice.ForPlayer _ -> "players"; _ -> "options") (Just source) [] (fmap Just (NonEmpty.toList choices) <> [Nothing | abstain])
  Prompt.ChooseCounterKind _ _ _ kinds -> choosing namer prompt "options" Nothing [] (NonEmpty.toList kinds)
  -- CR 122.5: the object the counters move off, and the one they move onto.
  Prompt.ChooseMovedCounterOrNone _ _ off onto kinds -> choosing namer prompt "options" Nothing [("off", object namer off), ("onto", object namer onto)] (orNone kinds)
  Prompt.ChooseDamageSource _ _ source candidates -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList candidates)
  -- CR 101.4b: the cards earlier choosers took, beside who took each.
  Prompt.ChooseCardInGraveyard _ _ source cards earlier -> choosing namer prompt "options" (Just source) [("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player Reply.object)) namer earlier)] (NonEmpty.toList cards)
  Prompt.ChooseCardInHand _ _ source cards -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList cards)
  Prompt.ChooseCardFromAmong _ _ source cards -> choosing namer prompt "options" (Just source) (departed namer gs (NonEmpty.toList cards)) (NonEmpty.toList cards)
  -- CR 309.2a: each dungeon by its printing, and under @"entries"@ its name.
  Prompt.ChooseDungeon _ _ dungeons -> choosing namer prompt "options" Nothing [("entries", R.Array (fmap (R.Text . printingName gs) (NonEmpty.toList dungeons)))] (NonEmpty.toList dungeons)
  Prompt.ChooseSticker _ _ source stickers -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList stickers)
  Prompt.ChooseStickerOrNone _ _ source stickers -> choosing namer prompt "options" (Just source) [] (orNone stickers)
  Prompt.ChooseNamePosition _ _ source positions -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList positions)
  -- CR 702.139a: each companion by its card, and under @"entries"@ its name,
  -- then none.
  Prompt.ChooseCompanion _ _ companions -> choosing namer prompt "options" Nothing [("entries", R.Array (fmap (R.Text . outsideName gs) (NonEmpty.toList companions) <> [R.Text (Text.pack "No companion")]))] (orNone companions)
  Prompt.ChooseRoom _ _ source rooms -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList rooms)
  Prompt.ChooseHalf _ _ source names -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList names)
  Prompt.ChooseLegend _ _ candidates -> choosing namer prompt "options" Nothing [] (NonEmpty.toList candidates)
  Prompt.ChooseAttackTarget _ _ attacker targets -> choosing namer prompt "options" (Just attacker) [] (NonEmpty.toList targets)
  Prompt.ChooseEnlist _ _ source candidates -> choosing namer prompt "options" (Just source) [] (orNone candidates)
  Prompt.ChooseEncode _ _ source candidates -> choosing namer prompt "options" (Just source) [] (orNone candidates)
  Prompt.ChooseRerolledDie _ _ source candidates -> choosing namer prompt "options" (Just source) [] (orNone candidates)
  Prompt.ChooseAssistant _ _ source players -> choosing namer prompt "players" (Just source) [] (orNone players)
  Prompt.ChooseCopyTarget _ _ source candidates -> choosing namer prompt "options" (Just source) [] (fmap Just candidates <> [Nothing])
  Prompt.ChooseRevealOnEntry _ _ source cards -> choosing namer prompt "options" (Just source) [] (orNone cards)
  Prompt.ChooseColor _ _ source colors -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList colors)
  Prompt.ChooseDesignation _ _ source designations -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList designations)
  Prompt.ChooseManaType _ _ source types -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList types)
  Prompt.ChooseCardType _ _ source types -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList types)
  Prompt.ChooseOpponent _ _ source _ players -> choosing namer prompt "players" (Just source) [] (NonEmpty.toList players)
  Prompt.ChooseProtector _ _ source players -> choosing namer prompt "players" (Just source) [] (NonEmpty.toList players)
  Prompt.ChoosePlayer _ _ source players -> choosing namer prompt "players" (Just source) [] (NonEmpty.toList players)
  Prompt.ChooseActivePlayer _ _ players -> choosing namer prompt "players" Nothing [] (NonEmpty.toList players)
  -- CR 101.4b: the objects earlier choosers chose, beside who chose them.
  Prompt.ChoosePermanent _ _ source candidates earlier -> choosing namer prompt "options" (Just source) [("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.setOf Reply.object))) namer earlier)] (NonEmpty.toList candidates)
  Prompt.ChooseCounterRemoval _ _ source candidates -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList candidates)
  Prompt.ChooseAttachment _ _ source candidates -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList candidates)
  Prompt.ChooseOfferedCastSpell _ _ offers -> choosing namer prompt "options" Nothing [] (NonEmpty.toList offers)
  Prompt.ChoosePlayPermission _ _ source permissions -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList permissions)
  Prompt.ChooseHeldPermission _ _ source permissions -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList permissions)
  -- CR 601.2b: the symbol announced for, beside each way to pay it.
  Prompt.AnnouncePhyrexianPayment _ _ source symbol payments -> choosing namer prompt "options" (Just source) [("symbol", Reply.encode (Reply.viaCodec Codec.ManaSymbol.codec) namer symbol)] (NonEmpty.toList payments)
  Prompt.AnnounceHybridPayment _ _ source mana payments -> choosing namer prompt "options" (Just source) [("mana", Reply.encode (Reply.viaCodec Codec.ManaType.codec) namer mana)] (NonEmpty.toList payments)
  Prompt.AnnounceHybridHalf _ _ source symbol halves -> choosing namer prompt "options" (Just source) [("symbol", Reply.encode (Reply.viaCodec Codec.ManaSymbol.codec) namer symbol)] (NonEmpty.toList halves)
  Prompt.ChooseHybridHalf _ _ source symbol halves -> choosing namer prompt "options" (Just source) [("symbol", Reply.encode (Reply.viaCodec Codec.ManaSymbol.codec) namer symbol)] (NonEmpty.toList halves)
  Prompt.ChooseReducedCost _ _ source costs -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList costs)
  -- CR 401.4 / 608.2c: from the top inward.
  Prompt.ArrangeLibraryCards _ _ cards -> listing namer "order" Nothing [] (fmap (object namer) cards)
  -- CR 401.4 / 608.2c: from the end they arrive at inward.
  Prompt.ArrangeLibraryArrivals _ _ end cards -> listing namer "order" Nothing [("end", Reply.encode (Reply.viaCodec Codec.LibraryPosition.codec) namer end)] (fmap (object namer) cards)
  -- CR 613.7m: the first named stamped earliest.
  Prompt.OrderTimestamps _ _ permanents -> listing namer "order" Nothing [] (fmap (object namer) permanents)
  -- CR 605.3: the first named activated first.
  Prompt.OrderManaActivations _ _ permanents -> listing namer "order" Nothing [] (fmap (object namer) permanents)
  -- CR 508.1j / 509.1f: each taxing permanent, the first named paid first.
  Prompt.OrderCombatTolls _ _ permanents -> listing namer "order" Nothing [] (fmap (object namer) permanents)
  -- CR 713.8a / 731.3a: each component card by name, the first named put into
  -- the zone first.
  Prompt.OrderComponentCards _ _ zone printings -> listing namer "order" Nothing [("zone", Reply.encode (Reply.viaCodec Codec.Zone.codec) namer zone)] (fmap (R.Text . printingName gs) printings)
  -- CR 601.2h: each non-mana component, the first named paid first.
  Prompt.OrderCostComponents _ _ source components -> listing namer "order" (Just source) [] (fmap (Reply.encode (Reply.viaCodec (Codec.CostComponent.codec Codec.Keyword.codec)) namer) components)
  -- CR 122.1c / 614.3: each event's source, recipient and amount.
  Prompt.OrderDamage _ _ events -> listing namer "order" Nothing [] (fmap (damageEvent namer) events)
  -- CR 608.2f / 701.44d / 707.10d: the first named handled first.
  Prompt.OrderForEach _ _ source recipients -> listing namer "order" (Just source) [] (fmap (Reply.encode Reply.recipient namer) recipients)
  -- CR 705.2.
  Prompt.CallCoin _ _ -> choosing namer prompt "options" Nothing [] [CoinFace.Heads, CoinFace.Tails]
  -- CR 701.30c: the seat's own card, every card revealed beside who revealed
  -- it, and (CR 101.4b) where earlier clashers put theirs.
  Prompt.ChooseClash _ _ source card revealed earlier ->
    choosing namer prompt "options" (Just source) [("card", object namer card), ("revealed", Reply.encode (Reply.list (Reply.pairOf Reply.player Reply.object)) namer (NonEmpty.toList revealed)), ("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.viaCodec Codec.LibraryPosition.codec))) namer earlier)] [LibraryPosition.Top, LibraryPosition.Bottom]
  -- CR 608.2d: how many cards stay above the upper option.
  Prompt.ChooseLibraryEnd _ _ source above -> choosing namer prompt "options" (Just source) [("above", natural above)] [LibraryPosition.Top, LibraryPosition.Bottom]
  Prompt.ChooseForage _ _ source -> choosing namer prompt "options" (Just source) [] [ForageMode.ExileCards, ForageMode.SacrificeFood]
  Prompt.ChooseParity _ _ source -> choosing namer prompt "options" (Just source) [] [Parity.Odd, Parity.Even]
  Prompt.ChooseWord _ _ source offered -> choosing namer prompt "options" (Just source) [] (NonEmpty.toList offered)
  -- CR 702.140c: the creature it merges with.
  Prompt.ChooseMutateSide _ _ source onto -> choosing namer prompt "options" (Just source) [("onto", object namer onto)] [MutateSide.Over, MutateSide.Under]
  Prompt.ChooseBuyback _ _ source cost -> choosing namer prompt "options" (Just source) [("cost", payment namer cost)] [BuybackDecision.BuysBack, BuybackDecision.Declines]
  Prompt.ChooseEntwine _ _ source cost -> choosing namer prompt "options" (Just source) [("cost", payment namer cost)] [EntwineDecision.Entwines, EntwineDecision.Declines]
  Prompt.ReturnCommander _ _ source -> choosing namer prompt "options" (Just source) [] [CommandZoneDecision.Returns, CommandZoneDecision.Leaves]
  Prompt.ChooseLandOrNonland _ _ source -> choosing namer prompt "options" (Just source) [] [Just LandOrNonland.Land, Just LandOrNonland.Nonland, Nothing]
  -- Each a yes or no, with what the seat decides about.
  Prompt.ChooseCommandZoneOfferFirst _ _ source -> choosing namer prompt "options" (Just source) [] yesOrNo
  Prompt.ChooseDredge _ _ source count -> choosing namer prompt "options" (Just source) [("count", natural count)] yesOrNo
  Prompt.ChooseExert _ _ source -> choosing namer prompt "options" (Just source) [] yesOrNo
  Prompt.ChooseExplore _ _ source card -> choosing namer prompt "options" (Just source) [("card", object namer card)] yesOrNo
  Prompt.ChoosePayLifeOnEntry _ _ source life -> choosing namer prompt "options" (Just source) [("life", natural life)] yesOrNo
  Prompt.ChooseRedirect _ _ source card -> choosing namer prompt "options" (Just source) [("card", object namer card)] yesOrNo
  -- How many times the body has run.
  Prompt.ChooseRepeat _ _ source times -> choosing namer prompt "options" (Just source) [("times", natural times)] yesOrNo
  Prompt.ChooseRiot _ _ source -> choosing namer prompt "options" (Just source) [] yesOrNo
  Prompt.ChooseSubstitute _ _ source -> choosing namer prompt "options" (Just source) [] yesOrNo
  Prompt.ChooseTribute _ _ source count -> choosing namer prompt "options" (Just source) [("count", natural count)] yesOrNo
  Prompt.ChooseTurnUpAttachment _ _ source -> choosing namer prompt "options" (Just source) [] yesOrNo
  Prompt.ChooseUnleash _ _ source -> choosing namer prompt "options" (Just source) [] yesOrNo
  Prompt.OfferedCast _ _ source name -> choosing namer prompt "options" (Just source) [("name", R.Text (CardName.unwrap name))] yesOrNo
  Prompt.OfferedMiracleReveal _ _ source name cost -> choosing namer prompt "options" (Just source) [("name", R.Text (CardName.unwrap name)), ("cost", payment namer cost)] yesOrNo
  Prompt.OfferedExtra _ _ source effect -> choosing namer prompt "options" (Just source) [("effect", Reply.encode (Reply.viaCodec Codec.PlayerEffect.codec) namer effect)] yesOrNo
  -- CR 706.2b: the results in roll order, each spelled as its index, and the
  -- one cost; none picked declines.
  Prompt.RerollDice _ _ results cost -> subset namer Nothing ([("results", R.Array (fmap natural (NonEmpty.toList results)))] <> [("cost", payment namer c) | Just c <- [cost]]) (Nothing, Nothing) (zipWith (\index _ -> natural index) [0 ..] (NonEmpty.toList results))
  -- CR 706.2a: the result rerolled, and what rerolling costs where it costs.
  Prompt.Rethrow _ _ landed cost -> choosing namer prompt "options" Nothing ([thrown landed] <> [("cost", payment namer c) | Just c <- [cost]]) yesOrNo
  -- CR 616.1: each entry as its source and, where it renders, its Oracle text.
  Prompt.ChooseReplacement _ _ entries -> indexing namer prompt Nothing [] (fmap (replacementEntry namer gs) entries)
  -- CR 208.2b / 614.1c: each shape's power, toughness, keywords and subtypes.
  Prompt.ChooseEntryOption _ _ source options -> indexing namer prompt (Just source) [] (fmap (Reply.encode (Reply.viaCodec Codec.EntryOption.codec) namer) options)
  -- CR 603.7b: each event, or null for one the seat did not witness.
  Prompt.ChooseDelayedTriggerEvent _ _ source events -> indexing namer prompt (Just source) [] (fmap (delayedEvent namer (Driver.deciderOf prompt)) (NonEmpty.toList events))
  -- CR 706.2b: each modifier by the object stating it, null for none.
  Prompt.ChooseRollModifier _ _ modifiers -> indexing namer prompt Nothing [] (fmap (\stated -> fields [("source", maybe R.Null (object namer) stated)]) (NonEmpty.toList modifiers))
  -- CR 123.5c: each object by its card's name.
  Prompt.KeepHand _ _ hands -> indexing namer prompt Nothing [] (fmap (objects namer) (NonEmpty.toList hands))
  Prompt.ChooseStickerKeeper _ _ printings -> indexing namer prompt Nothing [] (fmap (R.Text . printingName gs) (NonEmpty.toList printings))
  -- CR 614.1a: each token kind by its card's name.
  Prompt.ChooseTokenKind _ _ source kinds -> indexing namer prompt (Just source) [] (fmap (R.Text . CardName.unwrap) (NonEmpty.toList kinds))
  -- CR 706.4: each result as its number.
  -- CR 706.1a: the die by its sides.
  Prompt.RollDie sides -> fields [("sides", natural sides)]
  Prompt.ChooseDieResult _ _ source results -> indexing namer prompt source [] (fmap natural (NonEmpty.toList results))
  -- CR 701.48a: each branch, then neither.
  Prompt.ChooseLearn _ _ source modes -> choosing namer prompt "options" (Just source) [("entries", R.Array (fmap (R.Text . Text.pack . show) (NonEmpty.toList modes)))] (orNone modes)
  -- CR 608.2d / 701.55a: each branch by its Oracle text, then neither where it
  -- is offered; CR 101.4b: the branches earlier choosers announced, likewise.
  Prompt.ChooseClause _ _ source idx clauses neither earlier ->
    let branch = branchEntry gs source idx
     in choosing namer prompt "options" (Just source) [("earlier", R.Array [R.Array [Reply.encode Reply.player namer pid, maybe R.Null branch announced] | (pid, announced) <- Foldable.toList earlier]), ("entries", R.Array (fmap branch (NonEmpty.toList clauses)))] (fmap Just (NonEmpty.toList clauses) <> [Nothing | neither])
  -- CR 700.3 / 101.4b: each pile, and what earlier choosers took.
  Prompt.ChoosePile _ _ source piles earlier -> choosing namer prompt "options" (Just source) [("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.setOf Reply.object))) namer earlier)] (NonEmpty.toList piles)
  -- CR 103.5b / 103.6: each card's action, then none.
  Prompt.MulliganAction _ _ actions -> choosing namer prompt "options" Nothing [("entries", handActions namer actions)] (fmap Just actions <> [Nothing])
  Prompt.OpeningHandAction _ _ actions -> choosing namer prompt "options" Nothing [("entries", handActions namer actions)] (fmap Just actions <> [Nothing])
  -- CR 608.2g: each castable half and facing as a Cast action, then none.
  Prompt.CastWhileSearching _ _ casts -> choosing namer prompt "options" Nothing [("entries", R.Array [Offer.offer namer gs (Action.Cast oid name facing) | (oid, name, facing) <- casts])] (fmap Just casts <> [Nothing])
  -- Exactly the count, as many as there are where fewer (CR 701.21a).
  Prompt.ChooseSacrifices _ _ source candidates count earlier -> subset namer (Just source) [("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.setOf Reply.object))) namer earlier)] (exactly count candidates) (fmap (object namer) candidates)
  -- CR 118.1 / 406.2.
  Prompt.ChooseExiles _ _ source candidates count -> subset namer (Just source) [] (exactly count candidates) (fmap (object namer) candidates)
  Prompt.ChooseExilesFromGraveyard _ _ source candidates count -> subset namer (Just source) [] (exactly count candidates) (fmap (object namer) candidates)
  -- CR 118.1.
  Prompt.ChooseReturns _ _ source candidates count -> subset namer (Just source) [] (exactly count candidates) (fmap (object namer) candidates)
  -- CR 601.2f.
  Prompt.ChooseTaps _ _ source _ candidates count -> subset namer (Just source) [] (exactly count candidates) (fmap (object namer) candidates)
  -- CR 702.167a: "one or more" makes the count a least.
  Prompt.ChooseMaterials _ _ source candidates count orMore -> subset namer (Just source) [] (if orMore then (Just count, Nothing) else exactly count candidates) (fmap (object namer) candidates)
  -- CR 702.122a / 702.171a: any of them, whose total power the engine judges,
  -- and under @"counts"@ the power each counts as, a Pilot's "as though" raised.
  Prompt.ChooseTapsForTotalPower _ _ source candidates power -> subset namer (Just source) [("power", natural power), counts candidates] (Nothing, Nothing) (fmap (object namer . fst) candidates)
  -- CR 701.59a / 406.2: any of them, whose total tally the engine judges, and
  -- under @"counts"@ each one's tally; how the total compares where it is not
  -- a floor, and the fewest where there is one (Fabrication Foundry's).
  Prompt.ChooseExilesTotalling _ _ source tally candidates comparison total fewest ->
    subset namer (Just source) ([(tallyKey tally, natural total)] <> [("comparison", R.Text (Text.pack (show comparison))) | comparison /= Comparison.AtLeast] <> [counts candidates]) (if fewest > 0 then Just fewest else Nothing, Nothing) (fmap (object namer . fst) candidates)
  -- CR 608.2d: up to the most where there is one; CR 101.4b: what earlier
  -- choosers picked.
  Prompt.ChooseAnyNumber _ _ source purpose candidates most earlier ->
    subset namer (Just source) [("purpose", R.Text (Text.pack (show purpose))), ("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.setOf Reply.object))) namer earlier)] (Just 0, most) (fmap (object namer) candidates)
  -- CR 608.2d.
  Prompt.ChooseLoopMembers _ _ source howMany members -> subset namer (Just source) [] (case howMany of LoopMembers.One -> exactly 1 members; _ -> (Just 0, Nothing)) (fmap (Reply.encode Reply.recipient namer) members)
  -- CR 700.2a: the legal modes by index, each named, as many as the selection
  -- takes, (CR 700.2d) whether one may be chosen again, and (CR 700.2i) each
  -- one's pawprints, the selection's `most` being their budget.
  Prompt.ChooseModes _ _ source legal selection ->
    subset namer (Just source) (("names", R.Array (fmap (modeName gs source) (Set.toList legal))) : [("repeats", R.Boolean True) | Modal.allowsRepeatsIn selection] <> [("pawprints", R.Array (fmap natural weights)) | Just weights <- [traverse (Modal.pawprintsOf selection) (Set.toList legal)]]) (Just (Modal.leastOf selection), Modal.mostOf selection) (fmap (natural . ModeIndex.unwrap) (Set.toList legal))
  -- CR 702.47b: the cards in the order the answer names them, the first
  -- happening first, and under @"costs"@ the splice cost each adds.
  Prompt.ChooseSplice _ _ source offers -> subset namer (Just source) [("costs", R.Array (fmap (payment namer . snd) offers))] (Just 0, Nothing) (fmap (object namer . fst) offers)
  -- CR 701.23a / 701.23j: at least one place, each named.
  Prompt.ChooseSearchZones _ _ places ->
    subset namer Nothing [("names", R.Array (fmap (R.Text . placeName) (Set.toList places)))] (Just 1, Nothing) (fmap (Reply.encode Reply.searchPlace namer) (Set.toList places))
  -- CR 701.34a: the permanents, then the players as recipients.
  Prompt.ChooseProliferate _ _ permanents players -> subset namer Nothing [] (Just 0, Nothing) (fmap (object namer) permanents <> fmap (Reply.encode Reply.recipient namer . Recipient.ToPlayer) players)
  -- CR 400.11c: each card once per copy, named.
  Prompt.ChooseFromOutsideTheGame _ _ candidates least most ->
    let copies = concatMap (\(card, n) -> List.genericReplicate n card) (NonEmpty.toList candidates)
     in subset namer Nothing [("names", R.Array (fmap (R.Text . outsideName gs) copies))] (Just least, Just most) (fmap (Reply.encode (Reply.viaCodec Codec.OutsideCard.codec) namer) copies)
  -- CR 702.22c: the attackers, each in at most one band.
  Prompt.DeclareBands _ _ attackers -> listing namer "among" Nothing [] (fmap (object namer) attackers)
  -- CR 701.56a: the candidates, each added to, removed from or left alone.
  Prompt.ChooseTimeTravel _ _ source candidates -> listing namer "among" (Just source) [] (fmap (object namer) candidates)
  -- CR 701.22a / 701.25a / 701.29a: the cards looked at, top first, each
  -- named, and for a fateseal whose library they are.
  Prompt.ChooseScry _ _ cards -> looked namer gs [] cards
  Prompt.ChooseSurveil _ _ cards -> looked namer gs [] cards
  Prompt.ChooseFateseal _ _ owner cards -> looked namer gs [("owner", Reply.encode Reply.player namer owner)] cards
  -- CR 201.4 / 201.4a / 101.4b: no list, as the card reference is not one the
  -- engine holds; the restriction and the names earlier choosers chose.
  Prompt.ChooseCardName _ _ source restriction earlier ->
    fields
      [ ("source", object namer source),
        ("filter", Reply.encode (Reply.viaCodec (Codec.Filter.codec Codec.Keyword.codec)) namer restriction),
        ("earlier", Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.viaCodec Codec.CardName.codec))) namer earlier)
      ]
  -- CR 205.3: every member of the family, or of the card's list of them.
  Prompt.ChooseSubtype _ _ source family among -> choosing namer prompt "options" (Just source) [] (filter (Subtype.offered family among) [minBound ..])
  -- CR 612.2: the families the swap is drawn from and the words the new one
  -- may not be; not the pairs, which number in the tens of thousands for
  -- creature types.
  Prompt.ChooseWordSwap _ _ source slot families forbidden ->
    fields
      [ ("source", object namer source),
        ("slot", Reply.encode (Reply.viaCodec Codec.SlotName.codec) namer slot),
        ("families", Reply.encode (Reply.setOf (Reply.viaCodec Codec.WordFamily.codec)) namer families),
        ("forbidden", Reply.encode (Reply.setOf Reply.textWord) namer forbidden)
      ]
  _ -> R.Null

-- | A search place as the seat reads it.
placeName :: SearchPlace.SearchPlace -> Text.Text
placeName place = case place of
  SearchPlace.InZone zone -> zoneName zone
  SearchPlace.OutsideTheGame -> Text.pack "outside the game"

-- | CR 730.4a: a card outside the game by its name, and where it waits.
outsideName :: GameState.GameState -> OutsideCard.OutsideCard -> Text.Text
outsideName gs card = case card of
  OutsideCard.InPool printing -> printingName gs printing <> Text.pack " (sideboard)"
  OutsideCard.InAnotherGame oid -> foldMap (printingName gs . OutsideObject.printing) (Map.lookup oid (GameState.outsideObjects gs)) <> Text.pack " (game on hold)"

-- | Cards a seat looks at and splits into two piles: what else it is told,
-- each card under @"from"@ top first, and under @"names"@ each card's name.
looked :: Reply.Namer -> GameState.GameState -> [(String, R.Reply)] -> [ObjectId.ObjectId] -> R.Reply
looked namer gs told cards = fields (told <> [("from", objects namer cards), ("names", R.Array (fmap (R.Text . snapshotName . (`Projection.project` gs)) cards))])

-- | A prompt answered by a set of what it lists: its source, what else the
-- seat is told, the least and most it may pick where the rules bound them, and
-- under @"among"@ each candidate spelled as an element of the answer.
subset :: Reply.Namer -> Maybe ObjectId.ObjectId -> [(String, R.Reply)] -> (Maybe Natural, Maybe Natural) -> [R.Reply] -> R.Reply
subset namer source told (least, most) = listing namer "among" source (told <> [("least", natural n) | Just n <- [least]] <> [("most", natural n) | Just n <- [most]])

-- | A prompt answered by an amount per item: its source where it has one,
-- what else the seat is told, the least and most the amounts add up to where
-- the rules bound them, and under @"amounts"@ the answer it would be were each
-- item to take the most it may, so a key is spelled as the answer spells it.
amounts :: Reply.Namer -> Prompt.Prompt r -> Maybe ObjectId.ObjectId -> [(String, R.Reply)] -> (Maybe Natural, Maybe Natural) -> r -> R.Reply
amounts namer prompt source told (least, most) caps =
  fields ([("source", object namer s) | Just s <- [source]] <> told <> [("least", natural n) | Just n <- [least]] <> [("most", natural n) | Just n <- [most]] <> [("amounts", Reply.encode (Reply.shapeOf prompt) namer caps)])

-- | Exactly a count, or every candidate where there are fewer.
exactly :: Natural -> [a] -> (Maybe Natural, Maybe Natural)
exactly count candidates = let n = min count (Natural.length candidates) in (Just n, Just n)

-- | A yes or no, yes first.
yesOrNo :: [OptionalDecision.OptionalDecision]
yesOrNo = [OptionalDecision.Exercises, OptionalDecision.Declines]

-- | A cost in ChooseToPay's spelling.
payment :: Reply.Namer -> Cost.Type.Cost Keyword.Type.Keyword -> R.Reply
payment = Reply.encode (Reply.viaCodec (Codec.Cost.codec Codec.Keyword.codec))

-- | CR 101.4b: each earlier payer of one offer beside what they answered:
-- null for declining, or the option paid and the objects it took.
payerAnswers :: Reply.Namer -> Seq.Seq (PlayerId.PlayerId, PayerAnswer.PayerAnswer) -> R.Reply
payerAnswers namer = Reply.encode (Reply.seqOf (Reply.pairOf Reply.player (Reply.maybeOf (Reply.pairOf (Reply.viaCodec (Codec.Cost.codec Codec.Keyword.codec)) (Reply.setOf Reply.object))))) namer . fmap (fmap paid)
  where
    paid answer = case answer of
      PayerAnswer.Declined -> Nothing
      PayerAnswer.Paid cost taken -> Just (cost, taken)

-- | A prompt's source where it has one, what else the seat is told, and
-- under @key@ its items: the options `choosing` spells, or under @"order"@
-- what a prompt answered by a permutation of their indices orders
-- (Parun.Engine.Game.permute), each as the client names it.
listing :: Reply.Namer -> String -> Maybe ObjectId.ObjectId -> [(String, R.Reply)] -> [R.Reply] -> R.Reply
listing namer key source told items = fields ([("source", object namer s) | Just s <- [source]] <> told <> [(key, R.Array items)])

-- | A printing by its front face's name (Game.frontFaceOfPrinting), or empty
-- where the game knows no such printing.
printingName :: GameState.GameState -> PrintingId.PrintingId -> Text.Text
printingName gs = foldMap (CardName.unwrap . Face.name) . Seq.take 1 . Game.frontFaceOfPrinting gs

-- | A damage event as its source, recipient and amount; its riders say
-- nothing the seat orders by.
damageEvent :: Reply.Namer -> DamageEvent.DamageEvent -> R.Reply
damageEvent namer event = fields [("source", object namer (DamageEvent.source event)), ("target", Reply.encode Reply.recipient namer (DamageEvent.target event)), ("amount", natural (DamageEvent.amount event))]

-- | A prompt answered by one of what it lists: its source where it has one,
-- what else the seat is told, and under @key@ each option spelled as the
-- answer that picks it, @"players"@ for seats (which the line spells as bare
-- ids) and @"options"@ for anything else.
-- CR 706.2b's natural result as the die's "result", CR 705.1's face as the
-- coin's "face".
thrown :: ThrowResult.ThrowResult -> (String, R.Reply)
thrown landed = case landed of
  ThrowResult.DieCameUp result -> ("result", natural result)
  ThrowResult.CoinCameUp face -> ("face", R.Text (Text.pack (show face)))

choosing :: Reply.Namer -> Prompt.Prompt r -> String -> Maybe ObjectId.ObjectId -> [(String, R.Reply)] -> [r] -> R.Reply
choosing namer prompt key source told = listing namer key source told . fmap (Reply.encode (Reply.shapeOf prompt) namer)

-- | A prompt answered by an index into what it lists: its source where it
-- has one, what else the seat is told, each index under @"options"@, and under
-- @"entries"@ the entry each one picks, as the client names it.
indexing :: Reply.Namer -> Prompt.Prompt Natural -> Maybe ObjectId.ObjectId -> [(String, R.Reply)] -> [R.Reply] -> R.Reply
indexing namer prompt source told entries = choosing namer prompt "options" source (told <> [("entries", R.Array entries)]) (zipWith const [0 ..] entries)

-- | CR 616.1: a replacement entry as its source and, where it renders, the
-- Oracle text of its effect, which tells apart two effects of one source.
replacementEntry :: Reply.Namer -> GameState.GameState -> ReplacementEntry.ReplacementEntry -> R.Reply
replacementEntry namer gs entry =
  let source = ReplacementEntry.source entry
      self = snapshotName (Projection.project source gs)
      printed = PrintedReplacement.MkPrintedReplacement Nothing (ReplacementEntry.effect entry) Set.empty Nothing
   in fields (("source", object namer source) : [("text", R.Text text) | text <- take 1 (Oracle.Continuous.replacement (Oracle.contextOf self []) printed)])

-- | CR 700.2a: a mode as its Oracle text, as it reads now
-- (Resolve.resolvingMode) or, for a spell whose modes are not yet chosen, as
-- its face prints it; null where it does not render or renders empty.
modeName :: GameState.GameState -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> R.Reply
modeName gs source idx = maybe R.Null R.Text (List.find (not . Text.null) texts)
  where
    self = snapshotName (Projection.project source gs)
    printed = Projection.View.spellFaceOf source gs >>= Modal.modeAtIndex idx . Face.spell
    texts = foldMap (Oracle.Effect.mode (Oracle.contextOf self [])) (Foldable.asum [Resolve.resolvingMode source idx gs, printed])

-- | CR 608.2d: a branch as the Oracle text of its clause, its riders aside,
-- in the resolving object's mode as it reads now (Resolve.resolvingMode), or
-- as its index where the clause does not render. CR 701.55a's facing player
-- is "that player", as The Dalek Emperor prints it.
branchEntry :: GameState.GameState -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> ClauseIndex.ClauseIndex -> R.Reply
branchEntry gs source idx branch = maybe (natural (ClauseIndex.unwrap branch)) R.Text (Maybe.listToMaybe texts)
  where
    self = snapshotName (Projection.project source gs)
    texts = do
      mode <- Foldable.toList (Resolve.resolvingMode source idx gs)
      limb <- Foldable.toList (Seq.lookup (Natural.toIntSaturating (ClauseIndex.unwrap branch)) (Mode.clauses mode))
      let context = (Oracle.contextOf self []) {Oracle.Target.slots = Mode.targetSlots mode, Oracle.Target.bound = Map.singleton Binding.facingPlayers (Text.pack "that player")}
      fmap fst (Oracle.Effect.clause context limb {Clause.orElse = Nothing})

-- | CR 603.7b: an event the seat witnessed (Visibility.witnessed), or null.
delayedEvent :: Reply.Namer -> Maybe PlayerId.PlayerId -> GameEvent.GameEvent -> R.Reply
delayedEvent namer seat event
  | Just pid <- seat,
    Visibility.witnessed pid (LoggedEvent.MkLoggedEvent (EventGroup.MkEventGroup 0) event) == Visibility.Type.Visible =
      Reply.encode (Reply.viaCodec Codec.GameEvent.codec) namer event
  | otherwise = R.Null

-- | Each card's hand action by the card, and by its place among the card's
-- where the card offers more than one.
handActions :: Reply.Namer -> [(ObjectId.ObjectId, HandActionIndex.HandActionIndex)] -> R.Reply
handActions namer actions =
  R.Array
    [ fields (("source", object namer oid) : [("ability", natural (HandActionIndex.unwrap i)) | length (filter ((== oid) . fst) actions) > 1])
    | (oid, i) <- actions
    ]

-- | Each of a "may" prompt's options, then declining, spelled null.
orNone :: NonEmpty.NonEmpty a -> [Maybe a]
orNone options = fmap Just (NonEmpty.toList options) <> [Nothing]

-- | CR 603.3b: an entry of a trigger ordering as its source (null for none, CR
-- 113.7) and, where it renders, its ability's Oracle text, which tells apart
-- two abilities of one source.
triggerEntry :: Reply.Namer -> GameState.GameState -> TriggerEntry.TriggerEntry -> R.Reply
triggerEntry namer gs entry = fields (("source", source) : [("text", R.Text text) | text <- take 1 texts])
  where
    ability = TriggerEntry.ability entry
    (source, texts) = case TriggerEntry.source entry of
      TriggerSource.OfObject oid
        | Maybe.isJust (Game.lookupObject oid gs) ->
            let c = Projection.project oid gs
             in (object namer oid, Oracle.triggered (Oracle.contextOf (snapshotName c) []) (Set.member CardType.Creature (ProjectedCharacteristics.cardTypes c)) ability)
        | otherwise -> (object namer oid, [])
      TriggerSource.Sourceless -> (R.Null, Oracle.triggered (Oracle.contextOf Text.empty []) False ability)

-- | CR 702.51a / 702.66a / 702.126a: the keywords of the spell `payer` is
-- casting that let part of this cost be paid other than with mana, which
-- Cost.announceSubstitutions asks about once CR 601.2g's window closes. None
-- for a payment that is not a cast's: CR 601.2a put the spell on top of the
-- stack, and CR 601.2i has not yet logged it cast.
substitutingKeywords :: GameState.GameState -> PlayerId.PlayerId -> ManaCost.ManaCost -> [Keyword.Type.Keyword]
substitutingKeywords gs payer cost = case TurnRegisters.stack (GameState.turn gs) of
  top : _
    | Game.isSpell top gs,
      Projection.View.controllerOf top gs == Just payer,
      not (any (castAs top . LoggedEvent.event) (TurnRegisters.events (GameState.turn gs))) ->
        ListUtils.nubOrd
          [ k
          | k <- Map.keys (Cost.spellKeywords payer top gs),
            symbol <- ManaCost.unwrap cost,
            not (null (Keyword.manaSubstitutesFor symbol (Set.singleton k)))
          ]
  _ -> []
  where
    castAs top e = case e of
      GameEvent.SpellCast cast -> SpellWasCast.spell cast == top
      _ -> False

-- | Each mana source beside the mana it could make, one list of mana types
-- per yield (Mana.manaYieldsOf).
manaSources :: Reply.Namer -> GameState.GameState -> NonEmpty.NonEmpty ObjectId.ObjectId -> R.Reply
manaSources namer gs sources =
  R.Array
    [ fields [("source", object namer oid), ("makes", Reply.encode (Reply.list (Reply.list (Reply.viaCodec Codec.ManaType.codec))) namer (fmap (fmap ManaUnit.manaType . Mana.Type.unwrap) (Mana.manaYieldsOf oid gs)))]
    | oid <- Foldable.toList sources
    ]

-- | CR 608.2h: under @"departed"@, each of these whose id no longer names an
-- object (CR 400.7), which no view shows, as its id and its last known name.
-- Only one that left a public zone (CR 400.2), where every seat could see it;
-- one that left a hand or a library stays unnamed. Proved by
-- Parun.ProtocolSpec's "CR 608.2h a departed option".
departed :: Reply.Namer -> GameState.GameState -> [ObjectId.ObjectId] -> [(String, R.Reply)]
departed namer gs oids =
  [ ("departed", R.Object names)
  | let names =
          [ (Codec.Reference.toText (Reply.nameObject namer oid), R.Text (snapshotName (LastKnown.characteristics known)))
          | oid <- oids,
            Map.notMember oid (GameState.objects gs),
            Just known <- [Map.lookup oid (Snapshots.lastKnown (GameState.snapshots gs))],
            notElem (LastKnown.zone known) [Zone.Hand, Zone.Library]
          ],
    not (null names)
  ]

fields :: [(String, R.Reply)] -> R.Reply
fields = R.Object . fmap (Bifunctor.first Text.pack)

object :: Reply.Namer -> ObjectId.ObjectId -> R.Reply
object = Reply.encode Reply.object

objects :: Reply.Namer -> [ObjectId.ObjectId] -> R.Reply
objects = Reply.encode (Reply.list Reply.object)

natural :: Natural -> R.Reply
natural = R.Number . toInteger

-- | What each candidate of a threshold prompt adds toward its total, in the
-- order @"among"@ lists them.
counts :: [(ObjectId.ObjectId, Integer)] -> (String, R.Reply)
counts candidates = ("counts", R.Array (fmap (R.Number . snd) candidates))

-- | The field a threshold exile's total is told under: "manaValue", or
-- "blackManaSymbols" for Baron Helmut Zemo's boast.
tallyKey :: CardTally.CardTally -> String
tallyKey tally = case tally of
  CardTally.ManaValue -> "manaValue"
  CardTally.ManaSymbolsOf color -> fmap Char.toLower (show color) <> "ManaSymbols"

-- | CR 708.2a: a face-down object has no name.
snapshotName :: ProjectedCharacteristics.ProjectedCharacteristics -> Text.Text
snapshotName c = case Set.toList (ProjectedCharacteristics.names c) of
  [] -> Text.pack "a face-down card"
  names -> Text.intercalate (Text.pack " // ") (fmap CardName.unwrap names)

zoneName :: Zone.Zone -> Text.Text
zoneName z = Text.pack $ case z of
  Zone.Library -> "a library"
  Zone.Hand -> "a hand"
  Zone.Graveyard -> "a graveyard"
  Zone.Battlefield -> "the battlefield"
  Zone.Stack -> "the stack"
  Zone.Exile -> "exile"
  Zone.Command -> "the command zone"
  Zone.Ante -> "the ante"
