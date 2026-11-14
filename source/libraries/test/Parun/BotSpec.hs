{-# LANGUAGE GADTs #-}

-- Covers Parun.Play.Chance and Parun.Play.Bot: chance answered from a seeded
-- generator, and a seat answered at random among what its prompts offer.
module Parun.BotSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Word as Word
import Numeric.Natural (Natural)
import qualified Parun.Engine.Decide as Decide
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Monarch as Monarch
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Resolve.Effect as Resolve
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Subtype as Engine.Subtype
import qualified Parun.ManaSourceSpec as ManaSourceSpec
import qualified Parun.PlaneswalkerCombatSpec as PlaneswalkerCombatSpec
import qualified Parun.Play.Bot as Bot
import qualified Parun.Play.Chance as Chance
import qualified Parun.Registry as Registry
import qualified Parun.ReplaySpec as ReplaySpec
import qualified Parun.Scenario as Scenario
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.AnyNumberOf as AnyNumberOf
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.BallotChoice as BallotChoice
import qualified Parun.Types.BuybackDecision as BuybackDecision
import qualified Parun.Types.CandidateCost as CandidateCost
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CastFromZone as CastFromZone
import qualified Parun.Types.ChooseBetween as ChooseBetween
import qualified Parun.Types.ChoosePawprints as ChoosePawprints
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CommandZoneDecision as CommandZoneDecision
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterKindFor as CounterKindFor
import qualified Parun.Types.CounterSpread as CounterSpread
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Designation as Designation
import qualified Parun.Types.Dividing as Dividing
import qualified Parun.Types.EntryOption as EntryOption
import qualified Parun.Types.EntwineDecision as EntwineDecision
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.ForageMode as ForageMode
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator
import qualified Parun.Types.GraveyardArrangement as GraveyardArrangement
import qualified Parun.Types.GuessAnswer as GuessAnswer
import qualified Parun.Types.HandActionIndex as HandActionIndex
import qualified Parun.Types.HybridPayment as HybridPayment
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.InstructionRewrite as InstructionRewrite
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KickerDecision as KickerDecision
import qualified Parun.Types.LandOrNonland as LandOrNonland
import qualified Parun.Types.LearnMode as LearnMode
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.LifeChange as LifeChange
import qualified Parun.Types.LoopMembers as LoopMembers
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.MoveSpread as MoveSpread
import qualified Parun.Types.MutateSide as MutateSide
import qualified Parun.Types.NumberFloor as NumberFloor
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.OutsideCard as OutsideCard
import qualified Parun.Types.Parity as Parity
import qualified Parun.Types.PayOffer as PayOffer
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.PerformerR as PerformerR
import qualified Parun.Types.PermissionLimit as PermissionLimit
import qualified Parun.Types.PermissionPool as PermissionPool
import qualified Parun.Types.PermissionVerb as PermissionVerb
import qualified Parun.Types.PhyrexianPayment as PhyrexianPayment
import qualified Parun.Types.PlayerEffect as PlayerEffect
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerInstruction as PlayerInstruction
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.ReplacementEntry as ReplacementEntry
import qualified Parun.Types.Result as Result
import qualified Parun.Types.RollAdjustment as RollAdjustment
import qualified Parun.Types.RoomIndex as RoomIndex
import qualified Parun.Types.Scaling as Scaling
import qualified Parun.Types.SearchPlace as SearchPlace
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.StickerKind as StickerKind
import qualified Parun.Types.StickerRef as StickerRef
import qualified Parun.Types.StoredResult as StoredResult
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.SubtypeFamily as SubtypeFamily
import qualified Parun.Types.TargetCount as TargetCount
import qualified Parun.Types.TextWord as TextWord
import qualified Parun.Types.ThrowResult as ThrowResult
import qualified Parun.Types.TimeTravelChoice as TimeTravelChoice
import qualified Parun.Types.TriggerEntry as TriggerEntry
import qualified Parun.Types.TriggerSource as TriggerSource
import qualified Parun.Types.WordFamily as WordFamily
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Bot" $ do
  Spec.it s "chance shuffles each library from its seed" $ do
    matchup <- bears s registry
    let libraries gs = fmap (\pid -> Game.zoneMembers Zone.Library pid gs) [S.alice, S.bob]
        seeded seed = libraries (State.evalState (setUp Chance.chance matchup) (S.seeded seed))
        unshuffled = libraries (State.evalState (setUp (Answerer.MkAnswerer (pure . Replay.defaultAnswer . Asked.prompt)) matchup) (S.seeded 0))
    Spec.assertBool s (seeded 1 /= unshuffled) "shuffled"
    Spec.assertEq s (seeded 1) (seeded 1)
    Spec.assertBool s (seeded 1 /= seeded 2) "by seed"

  Spec.it s "two bots finish a game, the same game from the same seed" $ do
    matchup <- sevens s registry
    let ((result, gs), answers) = played 1 matchup
    Spec.assertBool s (GameState.result gs == Just result) "finished"
    Spec.assertBool s (all within answers) "within offer"
    Spec.assertEq s (fst (played 1 matchup)) (result, gs)

  Spec.it s "two bots answer only what each prompt offers, attacks among the answers" $ do
    matchup <- bears s registry
    -- Seed 3's game attacks on turn 5.
    let answers = snd (playedTo 6 3 matchup)
    Spec.assertBool s (all within answers) "within offer"
    Spec.assertBool s (any attacked answers) "attacked"

  Spec.it s "picks among every mana a source's ability can produce" $ do
    let options = ReplaySpec.oneMana Color.Red NonEmpty.:| [ReplaySpec.oneMana Color.Green, ReplaySpec.oneMana Color.Blue]
    Spec.assertEqWith s "yields" (drawn (Prompt.ChooseManaYield decider S.alice oid options)) (Set.fromList (NonEmpty.toList options))

  -- CR 201.4: any name it is handed; Parun.Interpreter.policingCardNames
  -- judges CR 201.4a, where the reference is.
  Spec.it s "CR 201.4 names a card from among the reference's names" $ do
    let names = fmap (CardName.MkCardName . Text.pack) ["Forest", "Grizzly Bears", "Hill Giant"]
    Spec.assertEqWith s "names" (drawnNaming names (Prompt.ChooseCardName decider S.alice oid (Filter.And []) Seq.empty)) (Set.fromList names)

  Spec.it s "puts triggers on the stack in every order" $ do
    let entries =
          [ TriggerEntry.MkTriggerEntry (TriggerSource.OfObject oid) Monarch.crownSteal,
            TriggerEntry.MkTriggerEntry TriggerSource.Sourceless Monarch.endStepDraw,
            TriggerEntry.MkTriggerEntry (TriggerSource.OfObject (ObjectId.MkObjectId 8)) Monarch.crownSteal
          ]
    Spec.assertEqWith s "orders" (drawn (Prompt.OrderTriggers decider S.alice entries)) (Set.fromList (List.permutations [0, 1, 2]))

  -- CR 510.1c / 702.19b: a trampler with power 4 blocked by creatures two and
  -- one short of lethal. Fifteen ways to divide 4 among three; the six legal
  -- ones keep the player at nothing until both blockers have lethal.
  Spec.it s "divides combat damage every legal way and no other" $ do
    let first = Recipient.ToCreature (ObjectId.MkObjectId 8)
        second = Recipient.ToCreature (ObjectId.MkObjectId 9)
        player = Recipient.ToPlayer S.bob
        thresholds :: Map.Map Recipient.Recipient Natural
        thresholds = Map.fromList [(first, 2), (second, 1), (player, 0)]
        legal :: [Map.Map Recipient.Recipient Natural]
        legal =
          fmap
            (Map.filter (> 0) . Map.fromList . zip [first, second, player])
            [[4, 0, 0], [3, 1, 0], [2, 2, 0], [1, 3, 0], [0, 4, 0], [2, 1, 1]]
    Spec.assertEqWith s "divisions" (drawn (Prompt.AssignCombatDamage decider S.alice oid thresholds Map.empty 4)) (Set.fromList legal)

  -- CR 702.19b's third sentence: the same trampler where other creatures
  -- already assign the first blocker its 2. Only the second blocker's 1 is
  -- left before the player, so eleven divisions are legal where six were.
  Spec.it s "divides combat damage leaning on what other creatures already assign" $ do
    let first = Recipient.ToCreature (ObjectId.MkObjectId 8)
        second = Recipient.ToCreature (ObjectId.MkObjectId 9)
        player = Recipient.ToPlayer S.bob
        thresholds :: Map.Map Recipient.Recipient Natural
        thresholds = Map.fromList [(first, 2), (second, 1), (player, 0)]
        legal :: [Map.Map Recipient.Recipient Natural]
        legal =
          fmap
            (Map.filter (> 0) . Map.fromList . zip [first, second, player])
            [[4, 0, 0], [3, 1, 0], [2, 2, 0], [1, 3, 0], [0, 4, 0], [2, 1, 1], [1, 2, 1], [0, 3, 1], [1, 1, 2], [0, 2, 2], [0, 1, 3]]
    Spec.assertEqWith s "divisions" (drawn (Prompt.AssignCombatDamage decider S.alice oid thresholds (Map.singleton first 2) 4)) (Set.fromList legal)

  -- The same through the engine: Typhoid Rats (1/1 deathtouch) or Llanowar
  -- Elves (1/1) announce 1 onto the 1/4 Palace Guard before the bot divides
  -- the 9/5 trampling Panglacial Wurm's damage over the Guard and bob. CR
  -- 702.2c makes the Rats' 1 the Guard's whole bar of 4, so any of the 9 may
  -- reach bob; the Elves' 1 leaves 3 owed. Read alone, the Wurm owes all 4.
  Spec.it s "CR 702.19b assigns trample damage another attacker's damage made legal" $ do
    rats <- S.printingOf s registry "Typhoid Rats"
    elves <- S.printingOf s registry "Llanowar Elves"
    wurm <- S.printingOf s registry "Panglacial Wurm"
    guard <- S.printingOf s registry "Palace Guard"
    let board first =
          let (gs, mine, theirs) = S.combatBoardOf [first, wurm] [guard]
           in (gs, mine, Maybe.fromMaybe S.noSource (Maybe.listToMaybe theirs))
        (deadly, deadlyAttackers, deadlyGuard) = board rats
        (plain, plainAttackers, plainGuard) = board elves
        run gs attackers seed = PlaneswalkerCombatSpec.runCombatLogging (botAssigning seed attackers) gs
        lives gs attackers = Set.fromList (fmap (S.lifeOf S.bob . fst . run gs attackers) [1 .. 128])
    Spec.assertEqWith s "anywhere from none to all 9 reach bob past the Rats" (lives deadly deadlyAttackers) (Set.fromList (fmap Just [11 .. 20]))
    Spec.assertEqWith s "and at most 6 past the Elves" (lives plain plainAttackers) (Set.fromList (fmap Just [14 .. 20]))
    -- The Wurm's offer, then the Guard's own division over the two attackers.
    Spec.assertEqWith s "the Wurm is offered the Rats' deathtouch as the Guard's whole bar" (snd (run deadly deadlyAttackers 1)) [Map.singleton (Recipient.ToCreature deadlyGuard) 4, Map.empty]
    Spec.assertEqWith s "and the Elves' 1 as 1 of it" (snd (run plain plainAttackers 1)) [Map.singleton (Recipient.ToCreature plainGuard) 1, Map.empty]

  -- CR 601.2d: 4 among three targets, each at least 1, is three ways; CR
  -- 115.7f's fixed parts 1 and 3 between two targets are two.
  Spec.it s "divides among targets every legal way and no other" $ do
    let first = Recipient.ToCreature (ObjectId.MkObjectId 8)
        second = Recipient.ToCreature (ObjectId.MkObjectId 9)
        player = Recipient.ToPlayer S.bob
        ways :: [[Natural]] -> [Recipient.Recipient] -> Set.Set (Map.Map Recipient.Recipient Natural)
        ways parts targets = Set.fromList (fmap (Map.fromList . zip targets) parts)
    Spec.assertEqWith s "totals" (drawn (Prompt.ChooseDivision decider S.alice oid (Dividing.Total 4) (first NonEmpty.:| [second, player]))) (ways [[2, 1, 1], [1, 2, 1], [1, 1, 2]] [first, second, player])
    Spec.assertEqWith s "parts" (drawn (Prompt.ChooseDivision decider S.alice oid (Dividing.Parts (1 NonEmpty.:| [3])) (first NonEmpty.:| [second]))) (ways [[1, 3], [3, 1]] [first, second])

  Spec.it s "pays or declines what a resolving spell offers" $ do
    let offer = PayOffer.AtClause (ModeIndex.MkModeIndex 0) (ClauseIndex.MkClauseIndex 0)
        cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []}
    Spec.assertEqWith s "decisions" (drawn (Prompt.ChooseToPay decider S.alice oid offer cost Seq.empty)) (Set.fromList [PaymentDecision.Declines, PaymentDecision.Pays])

  Spec.it s "taps another source or pays now once the cost is covered" $ do
    let sources = ObjectId.MkObjectId 8 NonEmpty.:| [ObjectId.MkObjectId 9]
    Spec.assertEqWith s "sources" (drawn (Prompt.ChooseExtraManaSource decider S.alice sources)) (Set.fromList (Nothing : fmap Just (NonEmpty.toList sources)))

  Spec.it s "spends any unit the pool offers" $ do
    let units = ManaSourceSpec.plainColor Color.Red NonEmpty.:| [ManaSourceSpec.plainColor Color.Green]
    Spec.assertEqWith s "units" (drawn (Prompt.ChooseManaToSpend decider S.alice units)) (Set.fromList (NonEmpty.toList units))

  -- CR 701.23b: a search for cards of a stated quality may find fewer, so
  -- every group of at most two of the three matches is an answer.
  Spec.it s "finds any group of at most so many matching cards" $ do
    let cards = fmap ObjectId.MkObjectId [8, 9, 10]
    Spec.assertEqWith s "groups" (Set.map List.sort (drawn (Prompt.Search decider S.alice cards 2))) (Set.fromList (filter ((<= 2) . length) (List.subsequences cards)))

  -- CR 608.2d: Imperial Edict's "a creature they control" is any of them.
  Spec.it s "chooses any matching permanent" $ do
    let candidates = ObjectId.MkObjectId 8 NonEmpty.:| [ObjectId.MkObjectId 9, ObjectId.MkObjectId 10]
    Spec.assertEqWith s "permanents" (drawn (Prompt.ChoosePermanent decider S.alice oid candidates Seq.empty)) (Set.fromList (NonEmpty.toList candidates))

  -- CR 608.2d: Coercion's "choose a card from it" is any of them.
  Spec.it s "chooses any card from among the group" $ do
    let candidates = ObjectId.MkObjectId 8 NonEmpty.:| [ObjectId.MkObjectId 9, ObjectId.MkObjectId 10]
    Spec.assertEqWith s "cards" (drawn (Prompt.ChooseCardFromAmong decider S.alice oid candidates)) (Set.fromList (NonEmpty.toList candidates))

  -- CR 701.21a: Wildfire's "sacrifices four lands" from five is any four.
  Spec.it s "sacrifices any group of the count" $ do
    let candidates = fmap ObjectId.MkObjectId [8 .. 12]
    Spec.assertEqWith s "groups" (drawn (Prompt.ChooseSacrifices decider S.alice oid candidates 4 Seq.empty)) (Set.fromList (fmap Set.fromList (filter ((== 4) . length) (List.subsequences candidates))))

  -- CR 508.1e / 702.22c: Mesa Pegasus's attack declares any disjoint bands
  -- of the attackers, none included.
  Spec.it s "declares any disjoint bands of the attackers" $ do
    let attackers = fmap ObjectId.MkObjectId [8 .. 11]
        bands = filter ((> 1) . length) (List.subsequences attackers)
        disjoint a b = null (List.intersect a b)
        declarations = [[]] <> fmap pure bands <> [[a, b] | a <- bands, b <- bands, a < b, disjoint a b]
    Spec.assertEqWith s "bands" (drawn (Prompt.DeclareBands decider S.alice attackers)) (Set.fromList (fmap (List.sort . fmap Set.fromList) declarations))

  -- CR 612.2: Sleight of Mind swaps any color word for any other.
  Spec.it s "swaps any two different words of the family" $ do
    let colors = fmap TextWord.ColorWord [minBound .. maxBound]
    Spec.assertEqWith s "swaps" (drawn (Prompt.ChooseWordSwap decider S.alice oid (SlotName.MkSlotName (Text.pack "target")) (Set.singleton WordFamily.Color) Set.empty)) (Set.fromList [(from, to) | from <- colors, to <- colors, from /= to])

  -- CR 105.4 / 106.3: Traitorous Greed's "two mana of any one color" is any
  -- of them.
  Spec.it s "adds any mana type offered" $ do
    let types = ManaType.Colored Color.White NonEmpty.:| fmap ManaType.Colored [Color.Blue, Color.Black, Color.Red, Color.Green]
    Spec.assertEqWith s "types" (drawn (Prompt.ChooseManaType decider S.alice oid types)) (Set.fromList (NonEmpty.toList types))

  -- CR 205.3m / 305.6: Adaptive Automaton names any creature type, of more
  -- than 256 seeds can draw; Convincing Mirage any of the five basic land
  -- types.
  Spec.it s "chooses any creature type or basic land type" $ do
    let creatureTypes = drawn (Prompt.ChooseSubtype decider S.alice oid SubtypeFamily.CreatureType Nothing)
    Spec.assertBool s (all Engine.Subtype.isCreatureType creatureTypes) "every creature type drawn is one"
    Spec.assertGtWith s "creature types" (Set.size creatureTypes) 100
    Spec.assertEqWith s "basic land types" (drawn (Prompt.ChooseSubtype decider S.alice oid SubtypeFamily.BasicLandType Nothing)) (Set.fromList [Subtype.Plains, Subtype.Island, Subtype.Swamp, Subtype.Mountain, Subtype.Forest])
    -- CR 614.12a: Eclipsed Realms lists its types, and only those are drawn.
    Spec.assertEqWith s "listed creature types" (drawn (Prompt.ChooseSubtype decider S.alice oid SubtypeFamily.CreatureType (Just (Subtype.Kithkin NonEmpty.:| [Subtype.Treefolk])))) (Set.fromList [Subtype.Kithkin, Subtype.Treefolk])

  -- CR 118.1 / 601.2h / 608.2d: Zameck Guildmage's counter from any bearer;
  -- Novijen Sages' two from among bearers of 2 and 1, any way that adds up;
  -- Mage-Ring Network's any number, at least the floor; Sensational
  -- Spider-Man's up to the cap.
  Spec.it s "removes counters any way the count allows" $ do
    let one = ObjectId.MkObjectId 8
        two = ObjectId.MkObjectId 9
        bearers = Map.fromList [(one, 2), (two, 1)] :: Map.Map ObjectId.ObjectId Natural
        every = [Map.filter (> 0) (Map.fromList [(one, a), (two, b)]) | a <- [0 .. 2], b <- [0 .. 1 :: Natural]]
        totalling p = Set.fromList (filter (p . sum) every)
    Spec.assertEqWith s "bearer" (drawn (Prompt.ChooseCounterRemoval decider S.alice oid (one NonEmpty.:| [two]))) (Set.fromList [one, two])
    Spec.assertEqWith s "among" (drawn (Prompt.ChooseCounterRemovalAmong decider S.alice oid 2 bearers)) (totalling (== 2))
    Spec.assertEqWith s "at least" (drawn (Prompt.ChooseCounterRemovalAtLeast decider S.alice oid 1 bearers)) (totalling (>= 1))
    Spec.assertEqWith s "up to" (drawn (Prompt.ChooseCounterRemovalUpTo decider S.alice oid 2 bearers)) (totalling (<= 2))

  -- CR 122.1 / 601.2h: two counters off creatures carrying a +1/+1 and a
  -- shield counter and a +1/+1 counter is three ways, at least two four, and
  -- two off one creature one.
  Spec.it s "removes counters by permanent and kind any way the spread allows" $ do
    let one = ObjectId.MkObjectId 8
        two = ObjectId.MkObjectId 9
        bearers :: Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural)
        bearers = Map.fromList [(one, Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Shield, 1)]), (two, Map.singleton CounterKind.PlusOnePlusOne 1)]
        every :: [(Natural, Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural))]
        every = [(a + b + c, Map.filter (not . Map.null) (Map.fromList [(one, Map.filter (> 0) (Map.fromList [(CounterKind.PlusOnePlusOne, a), (CounterKind.Shield, b)])), (two, Map.filter (> 0) (Map.singleton CounterKind.PlusOnePlusOne c))])) | a <- [0, 1], b <- [0, 1], c <- [0, 1 :: Natural]]
        totalling p = Set.fromList [division | (total, division) <- every, p total]
    Spec.assertEqWith s "among" (drawn (Prompt.ChooseMixedCounterRemoval decider S.alice oid CounterSpread.FromAmong 2 bearers)) (totalling (== 2))
    Spec.assertEqWith s "at least" (drawn (Prompt.ChooseMixedCounterRemoval decider S.alice oid CounterSpread.FromAmongAtLeast 2 bearers)) (totalling (>= 2))
    Spec.assertEqWith s "from one" (drawn (Prompt.ChooseMixedCounterRemoval decider S.alice oid CounterSpread.FromOne 2 bearers)) (Set.singleton (Map.singleton one (Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Shield, 1)])))

  -- CR 608.2d: three among two candidates, each named at least one.
  Spec.it s "distributes an amount every way that names each candidate at least once" $ do
    let one = ObjectId.MkObjectId 8
        two = ObjectId.MkObjectId 9
    Spec.assertEqWith s "distributions" (drawn (Prompt.ChooseDistribution decider S.alice oid 3 (one NonEmpty.:| [two]))) (Set.fromList [Map.singleton one 3, Map.singleton two 3, Map.fromList [(one, 1), (two, 2)], Map.fromList [(one, 2), (two, 1)]])

  -- CR 122.5 / 706.8b: two +1/+1 counters and a shield counter, moved any
  -- number or one or more; two stored results, rerolled any number.
  Spec.it s "moves or rerolls any number up to each tally" $ do
    let to = ObjectId.MkObjectId 9
        offered :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural
        offered = Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 1)]
        every :: [Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural]
        every = [Map.filter (> 0) (Map.fromList [(CounterKind.PlusOnePlusOne, a), (CounterKind.Shield, b)]) | a <- [0 .. 2], b <- [0, 1 :: Natural]]
        result = StoredResult.MkStoredResult {StoredResult.sides = 6, StoredResult.value = 5}
    Spec.assertEqWith s "any number" (drawn (Prompt.ChooseMovedCounters decider S.alice oid to offered)) (Set.fromList every)
    Spec.assertEqWith s "one or more" (drawn (Prompt.ChooseMovedCountersAtLeastOne decider S.alice oid to offered)) (Set.fromList (filter (not . Map.null) every))
    Spec.assertEqWith s "rerolls" (drawn (Prompt.ChooseStoredRerolls decider S.alice oid (Map.singleton result 2))) (Set.fromList [Map.empty, Map.singleton result 1, Map.singleton result 2])

  -- CR 122.5: two +1/+1 counters and a shield counter onto two recipients,
  -- eighteen ways in all, every one each spread allows.
  Spec.it s "moves counters onto several recipients any way the spread allows" $ do
    let one = ObjectId.MkObjectId 8
        two = ObjectId.MkObjectId 9
        offered :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural
        offered = Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 1)]
        onto at plus shield = (at, Map.filter (> 0) (Map.fromList [(CounterKind.PlusOnePlusOne, plus), (CounterKind.Shield, shield)]))
        every :: [((Natural, Natural), Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural))]
        every = [((pa + pb, sa + sb), Map.filter (not . Map.null) (Map.fromList [onto one pa sa, onto two pb sb])) | pa <- [0 .. 2], pb <- [0 .. 2 - pa], sa <- [0, 1], sb <- [0 .. 1 - sa :: Natural]]
        moving p = Set.fromList [answer | (totals, answer) <- every, p totals]
        spread kind = drawn (Prompt.ChooseDistributedMovedCounters decider S.alice oid kind offered (one NonEmpty.:| [two]))
    Spec.assertEqWith s "any number" (spread MoveSpread.AnyNumber) (moving (const True))
    Spec.assertEqWith s "one or more" (spread MoveSpread.AtLeastOne) (moving (/= (0, 0)))
    Spec.assertEqWith s "exactly" (spread MoveSpread.Exactly) (moving (== (2, 1)))
    Spec.assertEqWith s "one kind" (spread (MoveSpread.OneKind 2)) (moving (`elem` [(2, 0), (0, 1)]))
    Spec.assertEqWith s "up to one" (spread MoveSpread.UpToOne) (moving (\(plus, shield) -> plus + shield <= 1))

  Spec.it s "guesses either way" $ do
    Spec.assertEqWith s "guesses" (drawn (Prompt.ChooseGuess decider S.alice oid (Filter.And []))) (Set.fromList [GuessAnswer.Holds, GuessAnswer.DoesNotHold])

  -- CR 107.1c: a bounded number is any from its floor to its bound; an
  -- unbounded bid tops the high bid by one or declines.
  Spec.it s "chooses any number in range and tops a bid by one" $ do
    Spec.assertEqWith s "bounded" (drawn (Prompt.ChooseNumber decider S.alice oid 1 NumberFloor.Least (Just 4))) (Set.fromList [1 .. 4])
    Spec.assertEqWith s "bid" (drawn (Prompt.ChooseNumber decider S.alice oid 3 NumberFloor.HighBid Nothing)) (Set.fromList [3, 4])

  -- CR 601.2h: a total cost's components paid in any order; Ponder's cards
  -- put back in any.
  Spec.it s "orders cost components and library cards every way" $ do
    let components = [CostComponent.TapThis, CostComponent.SacrificeThis, CostComponent.ReturnThis]
        cards = fmap ObjectId.MkObjectId [8, 9, 10]
    Spec.assertEqWith s "components" (drawn (Prompt.OrderCostComponents decider S.alice oid components)) (Set.fromList (List.permutations [0, 1, 2]))
    Spec.assertEqWith s "arranged" (drawn (Prompt.ArrangeLibraryCards decider S.alice cards)) (Set.fromList (List.permutations [0, 1, 2]))

  -- CR 701.22a: scry 2 puts any of the two on the bottom and the rest on
  -- top, each group in any order: six answers.
  Spec.it s "scries any cards to the bottom in any order" $ do
    let one = ObjectId.MkObjectId 8
        two = ObjectId.MkObjectId 9
    Spec.assertEqWith s "scries" (drawn (Prompt.ChooseScry decider S.alice [one, two])) (Set.fromList [([], [one, two]), ([], [two, one]), ([one], [two]), ([two], [one]), ([one, two], []), ([two, one], [])])

  -- CR 700.2: "Choose one or both" over two legal modes is three selections;
  -- CR 700.2d's repeats make "choose two" over two modes three as well.
  Spec.it s "chooses every selection of modes the instruction allows" $ do
    let modes = Set.fromList (fmap ModeIndex.MkModeIndex [0, 1])
        selections = Set.fromList . fmap (Seq.fromList . fmap ModeIndex.MkModeIndex)
    Spec.assertEqWith s "range" (drawn (Prompt.ChooseModes decider S.alice oid modes (ModeSelection.ChooseBetween (ChooseBetween.MkChooseBetween 1 2)))) (selections [[0], [1], [0, 1]])
    Spec.assertEqWith s "repeats" (drawn (Prompt.ChooseModes decider S.alice oid modes (ModeSelection.ChooseExactlyWithRepeats 2))) (selections [[0, 0], [0, 1], [1, 1]])
    -- CR 700.2i: up to three pawprints over modes worth one and two.
    Spec.assertEqWith s "pawprints" (drawn (Prompt.ChooseModes decider S.alice oid modes (ModeSelection.ChoosePawprintsWithRepeats (ChoosePawprints.MkChoosePawprints 3 (Seq.fromList [1, 2]))))) (selections [[], [0], [1], [0, 0], [0, 1], [0, 0, 0]])

  -- CR 601.2c: "up to three targets" with two legal is zero to two.
  Spec.it s "announces any target count the board can supply" $ do
    let slot = SlotName.MkSlotName (Text.pack "target")
        legal = Set.fromList [Recipient.ToCreature (ObjectId.MkObjectId 8), Recipient.ToPlayer S.bob]
    Spec.assertEqWith s "counts" (drawn (Prompt.AnnounceTargets decider S.alice oid (Map.singleton slot (TargetCount.upTo 3, legal)))) (Set.fromList (fmap (Map.singleton slot) [0, 1, 2]))

  Spec.it s "takes or declines each offer" $ do
    let decisions = Set.fromList [OptionalDecision.Exercises, OptionalDecision.Declines]
    Spec.assertEqWith s "reversals" (drawn (Prompt.ReverseManaAbilities decider S.alice (oid NonEmpty.:| []))) decisions
    Spec.assertEqWith s "casts" (drawn (Prompt.OfferedCast decider S.alice oid (CardName.MkCardName (Text.pack "Grizzly Bears")))) decisions
    Spec.assertEqWith s "calls" (drawn (Prompt.CallCoin decider S.alice)) (Set.fromList [CoinFace.Heads, CoinFace.Tails])

  -- CR 608.2d: Alchemist's Gift's "deathtouch or lifelink" is either; Tolarian
  -- Kraken's "you may tap or untap" also neither.
  Spec.it s "chooses any branch of an or, or neither where offered" $ do
    let branches = fmap ClauseIndex.MkClauseIndex (0 NonEmpty.:| [1])
        clause neither = drawn (Prompt.ChooseClause decider S.alice oid (ModeIndex.MkModeIndex 0) branches neither Seq.empty)
        chosen = fmap Just (NonEmpty.toList branches)
    Spec.assertEqWith s "either" (clause False) (Set.fromList chosen)
    Spec.assertEqWith s "or neither" (clause True) (Set.fromList (Nothing : chosen))

  -- CR 614.1a: Island Sanctuary's "instead you may skip that draw", and every
  -- other "may" or either-or of the same answer, is taken or declined.
  Spec.it s "takes or declines each may" $ do
    let free = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}
        other = ObjectId.MkObjectId 8
        prompts =
          [ ("substitutes", Prompt.ChooseSubstitute decider S.alice oid),
            ("command zone offers", Prompt.ChooseCommandZoneOfferFirst decider S.alice oid),
            ("dredges", Prompt.ChooseDredge decider S.alice oid 2),
            ("exerts", Prompt.ChooseExert decider S.alice oid),
            ("explores", Prompt.ChooseExplore decider S.alice oid other),
            ("life on entry", Prompt.ChoosePayLifeOnEntry decider S.alice oid 2),
            ("redirects", Prompt.ChooseRedirect decider S.alice oid other),
            ("repeats", Prompt.ChooseRepeat decider S.alice oid 1),
            ("riots", Prompt.ChooseRiot decider S.alice oid),
            ("tributes", Prompt.ChooseTribute decider S.alice oid 2),
            ("attachments", Prompt.ChooseTurnUpAttachment decider S.alice oid),
            ("unleashes", Prompt.ChooseUnleash decider S.alice oid),
            ("extras", Prompt.OfferedExtra decider S.alice oid PlayerEffect.CantCastSpells),
            ("miracles", Prompt.OfferedMiracleReveal decider S.alice oid (CardName.MkCardName (Text.pack "Temporal Mastery")) free),
            ("rerolls", Prompt.Rethrow decider S.alice (ThrowResult.DieCameUp 3) Nothing)
          ]
        both = Set.fromList [OptionalDecision.Exercises, OptionalDecision.Declines]
    Spec.assertEqWith s "mays" (fmap (fmap drawn) prompts) (fmap (fmap (const both)) prompts)

  Spec.it s "chooses any one of what is offered" $ do
    let candidates = ObjectId.MkObjectId 8 NonEmpty.:| [ObjectId.MkObjectId 9]
        cheaper = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]), Cost.components = []}
        dearer = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []}
        costs = [CandidateCost.plain Nothing cheaper, CandidateCost.plain Nothing dearer]
        casts = (ObjectId.MkObjectId 8, CardName.MkCardName (Text.pack "Fire")) NonEmpty.:| [(ObjectId.MkObjectId 8, CardName.MkCardName (Text.pack "Ice"))]
    Spec.assertEqWith s "sources" (drawn (Prompt.ChooseDamageSource decider S.alice oid candidates)) (Set.fromList (NonEmpty.toList candidates))
    Spec.assertEqWith s "hand" (drawn (Prompt.ChooseCardInHand decider S.alice oid candidates)) (Set.fromList (NonEmpty.toList candidates))
    Spec.assertEqWith s "colors" (drawn (Prompt.ChooseColor decider S.alice oid (Color.Red NonEmpty.:| [Color.Green]))) (Set.fromList [Color.Red, Color.Green])
    Spec.assertEqWith s "costs" (drawn (Prompt.ChooseCost decider S.alice oid costs Seq.empty)) (Set.fromList costs)
    Spec.assertEqWith s "spells" (drawn (Prompt.ChooseOfferedCastSpell decider S.alice casts)) (Set.fromList (NonEmpty.toList casts))
    Spec.assertEqWith s "attacks" (drawn (Prompt.ChooseAttackTarget decider S.alice oid (AttackTarget.OfPlayer S.bob NonEmpty.:| [AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 8)]))) (Set.fromList [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 8)])

  -- CR 707.5: Clone may copy any candidate or nothing.
  Spec.it s "copies any candidate or declines" $ do
    let candidates = fmap ObjectId.MkObjectId [8, 9]
    Spec.assertEqWith s "copies" (drawn (Prompt.ChooseCopyTarget decider S.alice oid candidates)) (Set.fromList (Nothing : fmap Just candidates))

  -- Boonweaver Giant's "graveyard, hand, and/or library" is any nonempty
  -- group of the three.
  Spec.it s "searches any nonempty group of the places" $ do
    let places = Set.fromList (fmap SearchPlace.InZone [Zone.Graveyard, Zone.Hand, Zone.Library])
    Spec.assertEqWith s "places" (drawn (Prompt.ChooseSearchZones decider S.alice places)) (Set.fromList (fmap Set.fromList (filter (not . null) (List.subsequences (Set.toList places)))))

  -- CR 601.2f: tapping two of three is any two; "any number" of three, at
  -- most two, is any group of up to two.
  Spec.it s "taps any group of the count and takes any number up to the bound" $ do
    let candidates = fmap ObjectId.MkObjectId [8, 9, 10]
        groups p = Set.fromList (fmap Set.fromList (filter (p . length) (List.subsequences candidates)))
    Spec.assertEqWith s "taps" (drawn (Prompt.ChooseTaps decider S.alice oid candidates 2)) (groups (== 2))
    Spec.assertEqWith s "bounded" (drawn (Prompt.ChooseAnyNumber decider S.alice oid AnyNumberOf.CardsFromAmong candidates (Just 2) Seq.empty)) (groups (<= 2))
    Spec.assertEqWith s "unbounded" (drawn (Prompt.ChooseAnyNumber decider S.alice oid AnyNumberOf.Permanents candidates Nothing Seq.empty)) (groups (const True))

  -- CR 702.122a: crew 3 over powers 2, 1, -1 and 3 is any group reaching 3,
  -- the -1 only beside enough to cover it. CR 701.59a: evidence 4 over mana
  -- values 0, 2, 3 and 1 likewise. Out of reach, the fixed answer.
  Spec.it s "taps or exiles any group whose total reaches the threshold" $ do
    let candidates = fmap ObjectId.MkObjectId [8, 9, 10, 11]
        valued = zip candidates
        reaching :: [Integer] -> Integer -> Set.Set (Set.Set ObjectId.ObjectId)
        reaching values threshold = Set.fromList [Set.fromList (fmap fst group) | group <- List.subsequences (valued values), sum (fmap snd group) >= threshold]
    Spec.assertEqWith s "crews" (drawn (Prompt.ChooseTapsForTotalPower decider S.alice oid (valued [2, 1, -1, 3]) 3)) (reaching [2, 1, -1, 3] 3)
    Spec.assertEqWith s "collects" (drawn (Prompt.ChooseExilesTotalling decider S.alice oid CardTally.ManaValue (valued [0, 2, 3, 1]) 4)) (reaching [0, 2, 3, 1] 4)
    Spec.assertEqWith s "out of reach" (drawn (Prompt.ChooseTapsForTotalPower decider S.alice oid (valued [1, 1, -1, 0]) 3)) (Set.singleton (Set.fromList candidates))

  -- CR 615.7: a shield with 3 left over hits of 2 and 3 spends all 3, each
  -- hit taking at most its own damage.
  Spec.it s "allocates a shield every way that spends it" $ do
    let hit n = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.alice) n False False False 0 Nothing Nothing mempty False mempty DamageKind.Noncombat
    Spec.assertEqWith s "shares" (drawn (Prompt.AllocateDamage decider S.alice [hit 2, hit 3] 3)) (Set.fromList [[0, 3], [1, 2], [2, 1]])

  -- Each candidate the caller found legal, CR 118.13's payable routes included.
  Spec.it s "picks any one of the offer" $ do
    let objects = ObjectId.MkObjectId 8 NonEmpty.:| [ObjectId.MkObjectId 9]
        players = S.alice NonEmpty.:| [S.bob]
        printings = PrintingId.MkPrintingId 3 NonEmpty.:| [PrintingId.MkPrintingId 4]
        names = fmap (CardName.MkCardName . Text.pack) ("Fire" NonEmpty.:| ["Ice"])
        kinds = CounterKind.PlusOnePlusOne NonEmpty.:| [CounterKind.Shield]
        stickers = StickerRef.MkStickerRef S.alice 0 StickerKind.Art 0 NonEmpty.:| [StickerRef.MkStickerRef S.alice 1 StickerKind.Art 2]
        grant = CastFromZone.MkCastFromZone (InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.You)) (Filter.HasCardType CardType.Creature) PermissionLimit.Unlimited PermissionVerb.Play PermissionPool.EveryCard [] 0 ManaSpending.AsProduced Nothing Nothing Nothing
        permissions = Just (ObjectId.MkObjectId 8, grant) NonEmpty.:| [Nothing]
        every :: (Ord r) => NonEmpty.NonEmpty r -> Set.Set r
        every = Set.fromList . NonEmpty.toList
        green = ManaType.Colored Color.Green
        blue = ManaType.Colored Color.Blue
        halves = ManaSymbol.OfType (ManaType.Colored Color.Black) NonEmpty.:| [ManaSymbol.Generic 2]
        totals = fmap (ManaCost.MkManaCost . pure . ManaSymbol.Generic) (1 NonEmpty.:| [2])
        votes = BallotChoice.ForObject (ObjectId.MkObjectId 8) NonEmpty.:| [BallotChoice.ForPlayer S.bob]
    Spec.assertEqWith s "hybrid halves" (drawn (Prompt.AnnounceHybridHalf decider S.alice oid (ManaSymbol.Generic 1) (green NonEmpty.:| [blue]))) (Set.fromList [green, blue])
    Spec.assertEqWith s "hybrid payments" (drawn (Prompt.AnnounceHybridPayment decider S.alice oid green (HybridPayment.PaysTyped NonEmpty.:| [HybridPayment.PaysGeneric]))) (Set.fromList [HybridPayment.PaysTyped, HybridPayment.PaysGeneric])
    Spec.assertEqWith s "phyrexian payments" (drawn (Prompt.AnnouncePhyrexianPayment decider S.alice oid (ManaSymbol.Phyrexian Color.Green) (PhyrexianPayment.PaysMana NonEmpty.:| [PhyrexianPayment.PaysLife]))) (Set.fromList [PhyrexianPayment.PaysMana, PhyrexianPayment.PaysLife])
    Spec.assertEqWith s "active players" (drawn (Prompt.ChooseActivePlayer decider S.alice players)) (every players)
    Spec.assertEqWith s "amass" (drawn (Prompt.ChooseAmass decider S.alice oid objects)) (every objects)
    Spec.assertEqWith s "attachments" (drawn (Prompt.ChooseAttachment decider S.alice oid objects)) (every objects)
    Spec.assertEqWith s "beholds" (drawn (Prompt.ChooseBehold decider S.alice oid objects)) (every objects)
    Spec.assertEqWith s "blights" (drawn (Prompt.ChooseBlight decider S.alice oid objects)) (every objects)
    Spec.assertEqWith s "bolsters" (drawn (Prompt.ChooseBolster decider S.alice oid objects)) (every objects)
    Spec.assertEqWith s "graveyard cards" (drawn (Prompt.ChooseCardInGraveyard decider S.alice oid objects Seq.empty)) (every objects)
    Spec.assertEqWith s "card types" (drawn (Prompt.ChooseCardType decider S.alice oid (CardType.Artifact NonEmpty.:| [CardType.Land]))) (Set.fromList [CardType.Artifact, CardType.Land])
    Spec.assertEqWith s "coin results" (drawn (Prompt.ChooseCoinResult decider S.alice (CoinFace.Heads NonEmpty.:| [CoinFace.Tails]))) (Set.fromList [CoinFace.Heads, CoinFace.Tails])
    Spec.assertEqWith s "conjured cards" (drawn (Prompt.ChooseConjuredCard decider S.alice names)) (every names)
    Spec.assertEqWith s "counter kinds" (drawn (Prompt.ChooseCounterKind decider S.alice (CounterKindFor.MovedBetween oid (ObjectId.MkObjectId 9)) kinds)) (every kinds)
    Spec.assertEqWith s "defenders" (drawn (Prompt.ChooseDefender decider S.alice players)) (every players)
    Spec.assertEqWith s "designations" (drawn (Prompt.ChooseDesignation decider S.alice oid (Designation.AlphaSector NonEmpty.:| [Designation.BetaSector, Designation.GammaSector]))) (Set.fromList [Designation.AlphaSector, Designation.BetaSector, Designation.GammaSector])
    Spec.assertEqWith s "dungeons" (drawn (Prompt.ChooseDungeon decider S.alice printings)) (every printings)
    Spec.assertEqWith s "halves" (drawn (Prompt.ChooseHalf decider S.alice oid names)) (every names)
    let anchors = fmap Text.pack ("Khans" NonEmpty.:| ["Dragons"])
    Spec.assertEqWith s "words" (drawn (Prompt.ChooseWord decider S.alice oid anchors)) (every anchors)
    Spec.assertEqWith s "legends" (drawn (Prompt.ChooseLegend decider S.alice objects)) (every objects)
    Spec.assertEqWith s "name positions" (drawn (Prompt.ChooseNamePosition decider S.alice oid (0 NonEmpty.:| [1, 2]))) (Set.fromList [0, 1, 2])
    Spec.assertEqWith s "opponents" (drawn (Prompt.ChooseOpponent decider S.alice oid Nothing players)) (every players)
    Spec.assertEqWith s "permissions" (drawn (Prompt.ChoosePlayPermission decider S.alice oid permissions)) (every permissions)
    Spec.assertEqWith s "players" (drawn (Prompt.ChoosePlayer decider S.alice oid players)) (every players)
    Spec.assertEqWith s "protectors" (drawn (Prompt.ChooseProtector decider S.alice oid players)) (every players)
    Spec.assertEqWith s "reduced costs" (drawn (Prompt.ChooseReducedCost decider S.alice oid totals)) (every totals)
    Spec.assertEqWith s "reduction halves" (drawn (Prompt.ChooseReductionHalf decider S.alice oid (ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Black)) halves)) (every halves)
    Spec.assertEqWith s "ring-bearers" (drawn (Prompt.ChooseRingBearer decider S.alice objects)) (every objects)
    Spec.assertEqWith s "rooms" (drawn (Prompt.ChooseRoom decider S.alice oid (RoomIndex.MkRoomIndex 1 NonEmpty.:| [RoomIndex.MkRoomIndex 2]))) (Set.fromList [RoomIndex.MkRoomIndex 1, RoomIndex.MkRoomIndex 2])
    Spec.assertEqWith s "stickers" (drawn (Prompt.ChooseSticker decider S.alice oid stickers)) (every stickers)
    Spec.assertEqWith s "votes" (drawn (Prompt.ChooseVote decider S.alice oid votes)) (every votes)

  -- An index answers each of these: every position in the offer, equal
  -- entries included.
  Spec.it s "picks any index into the offer" $ do
    let gained who = GameEvent.LifeGained (LifeChange.MkLifeChange who 4 True)
        shape power = EntryOption.MkEntryOption {EntryOption.power = power, EntryOption.toughness = 3, EntryOption.keywords = Set.empty, EntryOption.subtypes = Set.empty}
        entry source = ReplacementEntry.MkReplacementEntry (ObjectId.MkObjectId source) (ReplacementEffect.PerformerR (PerformerR.MkPerformerR PlayerInstruction.Assemble (Filter.And []) (InstructionRewrite.Scaled (Scaling.Multiply 2))))
    Spec.assertEqWith s "occurrences" (drawn (Prompt.ChooseDelayedTriggerEvent decider S.alice oid (gained S.alice NonEmpty.:| [gained S.bob]))) (Set.fromList [0, 1])
    Spec.assertEqWith s "die results" (drawn (Prompt.ChooseDieResult decider S.alice Nothing (3 NonEmpty.:| [5, 5]))) (Set.fromList [0, 1, 2])
    Spec.assertEqWith s "entry options" (drawn (Prompt.ChooseEntryOption decider S.alice oid [shape 3, shape 2])) (Set.fromList [0, 1])
    Spec.assertEqWith s "replacements" (drawn (Prompt.ChooseReplacement decider S.alice [entry 8, entry 9, entry 10])) (Set.fromList [0, 1, 2])
    Spec.assertEqWith s "roll modifiers" (drawn (Prompt.ChooseRollModifier decider S.alice (Just oid NonEmpty.:| [Nothing]))) (Set.fromList [0, 1])
    Spec.assertEqWith s "opening hands" (drawn (Prompt.KeepHand decider S.alice ([oid] NonEmpty.:| [[], [oid]]))) (Set.fromList [0, 1, 2])
    Spec.assertEqWith s "sticker keepers" (drawn (Prompt.ChooseStickerKeeper decider S.alice (PrintingId.MkPrintingId 3 NonEmpty.:| [PrintingId.MkPrintingId 4]))) (Set.fromList [0, 1])

  -- Each is a "may", an "up to one" or a "wish to", so declining is an
  -- answer beside every candidate.
  -- CR 406.2 / 702.167a / 608.2d / 701.34a: exactly two of three, at least
  -- two, or any number; proliferate over objects and players.
  Spec.it s "chooses any group the count allows" $ do
    let cards = fmap ObjectId.MkObjectId [8, 9, 10]
        groups p = Set.fromList (fmap Set.fromList (filter (p . length) (List.subsequences cards)))
        recipients = [Recipient.ToCreature (ObjectId.MkObjectId 8), Recipient.ToPlayer S.bob]
        objects = take 2 cards
    Spec.assertEqWith s "exiles" (drawn (Prompt.ChooseExilesFromGraveyard decider S.alice oid cards 2)) (groups (== 2))
    Spec.assertEqWith s "materials" (drawn (Prompt.ChooseMaterials decider S.alice oid cards 2 False)) (groups (== 2))
    Spec.assertEqWith s "one or more materials" (drawn (Prompt.ChooseMaterials decider S.alice oid cards 2 True)) (groups (>= 2))
    Spec.assertEqWith s "loop members" (drawn (Prompt.ChooseLoopMembers decider S.alice oid LoopMembers.AnyNumber recipients)) (Set.fromList (fmap Set.fromList (List.subsequences recipients)))
    Spec.assertEqWith s "one loop member" (drawn (Prompt.ChooseLoopMembers decider S.alice oid LoopMembers.One recipients)) (Set.fromList (fmap Set.singleton recipients))
    Spec.assertEqWith s "proliferates" (drawn (Prompt.ChooseProliferate decider S.alice objects [S.alice, S.bob])) (Set.fromList [(Set.fromList os, Set.fromList ps) | os <- List.subsequences objects, ps <- List.subsequences [S.alice, S.bob]])

  -- CR 701.56a: each of two objects left alone, given a counter or losing one;
  -- CR 400.11c: one or two cards from two offers, either named twice.
  Spec.it s "time travels any way and brings in any cards from outside" $ do
    let one = ObjectId.MkObjectId 8
        two = ObjectId.MkObjectId 9
        ways = [Nothing, Just TimeTravelChoice.Add, Just TimeTravelChoice.Remove]
        outside = (OutsideCard.InPool (PrintingId.MkPrintingId 7), 1 :: Natural) NonEmpty.:| [(OutsideCard.InPool (PrintingId.MkPrintingId 8), 1)]
        offers = fmap fst (NonEmpty.toList outside)
    Spec.assertEqWith s "time travels" (drawn (Prompt.ChooseTimeTravel decider S.alice oid [one, two])) (Set.fromList [Map.fromList (Maybe.catMaybes [fmap ((,) one) a, fmap ((,) two) b]) | a <- ways, b <- ways])
    Spec.assertEqWith s "outside" (drawn (Prompt.ChooseFromOutsideTheGame decider S.alice outside 1 2)) (Set.fromList (fmap pure offers <> [[a, b] | a <- offers, b <- offers]))

  -- CR 700.3: Fact or Fiction's either pile; Camouflage's three creatures in
  -- two piles, either possibly empty (CR 700.3d).
  Spec.it s "takes any pile and divides into piles every way" $ do
    let first = Set.fromList (fmap ObjectId.MkObjectId [8, 9])
        second = Set.singleton (ObjectId.MkObjectId 10)
        creatures = fmap ObjectId.MkObjectId [8, 9, 10]
        divided = [[Set.fromList [c | (c, at) <- zip creatures ats, at == pile] | pile <- [0, 1 :: Int]] | ats <- Monad.replicateM 3 [0, 1]]
    Spec.assertEqWith s "piles" (drawn (Prompt.ChoosePile decider S.alice oid (first NonEmpty.:| [second]) Seq.empty)) (Set.fromList [first, second])
    Spec.assertEqWith s "divisions" (drawn (Prompt.DividePiles decider S.alice creatures 2)) (Set.fromList divided)

  -- CR 404.3: three cards in any order; CR 702.47a/b: any of two splice
  -- offers in any order; Reverse the Sands' any permutation of any group.
  Spec.it s "arranges, splices and redistributes in any order" $ do
    let cards = fmap ObjectId.MkObjectId [8, 9, 10]
        one = ObjectId.MkObjectId 8
        two = ObjectId.MkObjectId 9
        free = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}
        swaps = [Map.empty, Map.singleton S.alice S.alice, Map.singleton S.bob S.bob, Map.fromList [(S.alice, S.alice), (S.bob, S.bob)], Map.fromList [(S.alice, S.bob), (S.bob, S.alice)]]
    Spec.assertEqWith s "arrivals" (drawn (Prompt.ArrangeGraveyardArrivals decider S.alice cards)) (Set.fromList (fmap GraveyardArrangement.InOrder (List.permutations [0, 1, 2])))
    Spec.assertEqWith s "splices" (drawn (Prompt.ChooseSplice decider S.alice oid [(one, free), (two, free)])) (Set.fromList [[], [one], [two], [one, two], [two, one]])
    Spec.assertEqWith s "redistributions" (drawn (Prompt.ChooseRedistribution decider S.alice [(S.alice, 20), (S.bob, 10)])) (Set.fromList swaps)

  Spec.it s "picks any one of the offer or none" $ do
    let objects = ObjectId.MkObjectId 8 NonEmpty.:| [ObjectId.MkObjectId 9]
        orNone :: (Foldable t, Ord a) => t a -> Set.Set (Maybe a)
        orNone items = Set.fromList (Nothing : fmap Just (Foldable.toList items))
        casts = [(ObjectId.MkObjectId 8, CardName.MkCardName (Text.pack "Panglacial Wurm"), Facing.FaceUp)]
        companions = OutsideCard.InPool (PrintingId.MkPrintingId 7) NonEmpty.:| [OutsideCard.InAnotherGame (ObjectId.MkObjectId 9)]
        kinds = CounterKind.PlusOnePlusOne NonEmpty.:| [CounterKind.Shield]
        stickers = StickerRef.MkStickerRef S.alice 0 StickerKind.Art 0 NonEmpty.:| []
        actions = [(ObjectId.MkObjectId 7, HandActionIndex.MkHandActionIndex 0), (ObjectId.MkObjectId 7, HandActionIndex.MkHandActionIndex 1)]
    Spec.assertEqWith s "casts" (drawn (Prompt.CastWhileSearching decider S.alice casts)) (orNone casts)
    Spec.assertEqWith s "assistants" (drawn (Prompt.ChooseAssistant decider S.alice oid (S.bob NonEmpty.:| []))) (orNone [S.bob])
    Spec.assertEqWith s "companions" (drawn (Prompt.ChooseCompanion decider S.alice companions)) (orNone companions)
    Spec.assertEqWith s "encodes" (drawn (Prompt.ChooseEncode decider S.alice oid objects)) (orNone objects)
    Spec.assertEqWith s "enlists" (drawn (Prompt.ChooseEnlist decider S.alice oid objects)) (orNone objects)
    Spec.assertEqWith s "learns" (drawn (Prompt.ChooseLearn decider S.alice oid (LearnMode.TakeLesson NonEmpty.:| [LearnMode.DiscardAndDraw]))) (orNone [LearnMode.TakeLesson, LearnMode.DiscardAndDraw])
    Spec.assertEqWith s "moved counters" (drawn (Prompt.ChooseMovedCounterOrNone decider S.alice oid (ObjectId.MkObjectId 9) kinds)) (orNone kinds)
    Spec.assertEqWith s "reveals" (drawn (Prompt.ChooseRevealOnEntry decider S.alice oid objects)) (orNone objects)
    Spec.assertEqWith s "stickers" (drawn (Prompt.ChooseStickerOrNone decider S.alice oid stickers)) (orNone stickers)
    Spec.assertEqWith s "mulligan actions" (drawn (Prompt.MulliganAction decider S.alice actions)) (orNone actions)
    Spec.assertEqWith s "opening hand actions" (drawn (Prompt.OpeningHandAction decider S.alice actions)) (orNone actions)

  Spec.it s "picks any of a fixed set of decisions or a number up to its bound" $ do
    let free = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}
        ends = Set.fromList [LibraryPosition.Top, LibraryPosition.Bottom]
        kicks limit = drawn (Prompt.ChooseKicker decider S.alice oid (Keyword.Multikicker free) Nothing limit)
    Spec.assertEqWith s "buybacks" (drawn (Prompt.ChooseBuyback decider S.alice oid free)) (Set.fromList [BuybackDecision.Declines, BuybackDecision.BuysBack])
    Spec.assertEqWith s "entwines" (drawn (Prompt.ChooseEntwine decider S.alice oid free)) (Set.fromList [EntwineDecision.Declines, EntwineDecision.Entwines])
    Spec.assertEqWith s "forages" (drawn (Prompt.ChooseForage decider S.alice oid)) (Set.fromList [ForageMode.ExileCards, ForageMode.SacrificeFood])
    Spec.assertEqWith s "parities" (drawn (Prompt.ChooseParity decider S.alice oid)) (Set.fromList [Parity.Even, Parity.Odd])
    Spec.assertEqWith s "mutate sides" (drawn (Prompt.ChooseMutateSide decider S.alice oid (ObjectId.MkObjectId 8))) (Set.fromList [MutateSide.Over, MutateSide.Under])
    Spec.assertEqWith s "library ends" (drawn (Prompt.ChooseLibraryEnd decider S.alice oid 0)) ends
    Spec.assertEqWith s "clashes" (drawn (Prompt.ChooseClash decider S.alice oid (ObjectId.MkObjectId 8) ((S.alice, ObjectId.MkObjectId 8) NonEmpty.:| []) Seq.empty)) ends
    Spec.assertEqWith s "commanders" (drawn (Prompt.ReturnCommander decider S.alice oid)) (Set.fromList [CommandZoneDecision.Leaves, CommandZoneDecision.Returns])
    Spec.assertEqWith s "land or nonland" (drawn (Prompt.ChooseLandOrNonland decider S.alice oid)) (Set.fromList [Nothing, Just LandOrNonland.Land, Just LandOrNonland.Nonland])
    Spec.assertEqWith s "limited kicks" (kicks (Just 3)) (Set.fromList (fmap KickerDecision.MkKickerDecision [0 .. 3]))
    Spec.assertEqWith s "unlimited kicks" (kicks Nothing) (Set.fromList (fmap KickerDecision.MkKickerDecision [0, 1]))
    Spec.assertEqWith s "assist amounts" (drawn (Prompt.ChooseAssistAmount decider S.alice oid 3)) (Set.fromList [0 .. 3])
    Spec.assertEqWith s "paid energy" (drawn (Prompt.ChoosePaidEnergy decider S.alice oid 3)) (Set.fromList [0 .. 3])
    Spec.assertEqWith s "chapters" (drawn (Prompt.ChooseReadAheadChapter decider S.alice oid 3)) (Set.fromList [1 .. 3])
    Spec.assertEqWith s "die adjustments" (drawn (Prompt.AdjustDieRoll decider S.alice (4 NonEmpty.:| [-1]) 1 Nothing)) (Set.fromList (Nothing : [Just (index, adjustment) | index <- [0, 1], adjustment <- [RollAdjustment.Increase, RollAdjustment.Decrease]]))

-- Every answer the bot gives the prompt over many seeds.
drawn :: (Ord r) => Prompt.Prompt r -> Set.Set r
drawn = drawnNaming []

-- Every answer the bot gives the prompt over many seeds, naming a card from
-- `names`.
drawnNaming :: (Ord r) => [CardName.CardName] -> Prompt.Prompt r -> Set.Set r
drawnNaming names prompt = Set.fromList (fmap (State.evalState (Answerer.unwrap (Bot.bot names) (Asked.mapGames (PlayerView.viewFor S.alice) (Asked.MkAsked [] (Setup.emptyGame S.bothPlayers) prompt))) . S.seeded) [1 .. 256])

-- The bot dividing every creature's combat damage from `seed`, recording what
-- each offer says the step's other attacking creatures already pay; the
-- Guard-style block of every attacker otherwise.
botAssigning :: Word.Word32 -> [ObjectId.ObjectId] -> Prompt.Prompt r -> State.State [Map.Map Recipient.Recipient Natural] r
botAssigning seed attackers p = case p of
  Prompt.AssignCombatDamage _ _ _ _ paid _ -> do
    State.modify' (<> [paid])
    pure (State.evalState (Answerer.unwrap (Bot.bot []) (Asked.mapGames (PlayerView.viewFor S.alice) (Asked.MkAsked [] (Setup.emptyGame S.bothPlayers) p))) (S.seeded seed))
  _ -> pure (PlaneswalkerCombatSpec.blockingAll attackers p)

decider :: Decider.Decider
decider = Decide.deciderFor S.alice (Setup.emptyGame S.bothPlayers)

oid :: ObjectId.ObjectId
oid = ObjectId.MkObjectId 7

-- One seated answer: whether the prompt offered it, and whether it declared
-- an attack.
data Answered = MkAnswered {within :: Bool, attacked :: Bool}

played :: Word.Word32 -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> ((Result.Result, GameState.GameState), [Answered])
played seed matchup = State.runState (State.evalStateT (Driver.match [watching] seats GameSettings.plain Map.empty matchup) (S.seeded seed)) []

-- played's game stopped where turn `turn` ends, with no result to wait for.
playedTo :: Natural -> Word.Word32 -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [Answered])
playedTo turn seed matchup = State.runState (State.evalStateT (S.playedThrough turn [watching] seats matchup) (S.seeded seed)) []

-- The random bot in both seats.
seats :: Answerer.Answerer (Asked.Asked GameState.GameState) (State.StateT Generator.Generator (State.State [Answered]))
seats = Driver.bySeat (Map.fromList [(S.alice, Driver.seated (Bot.bot []) Chance.chance), (S.bob, Driver.seated (Bot.bot []) Chance.chance)]) Chance.chance

watching :: Observer.Observer (State.StateT Generator.Generator (State.State [Answered]))
watching =
  Observer.MkObserver
    { Observer.before = \_ -> pure (),
      Observer.after = \asked answer ->
        let prompt = Asked.prompt asked
         in case Driver.deciderOf prompt of
              Nothing -> pure ()
              Just _ -> Trans.lift (State.modify' (<> [MkAnswered (Scenario.withinOffer prompt answer) (declares prompt answer)]))
    }

declares :: Prompt.Prompt r -> r -> Bool
declares prompt answer = case prompt of
  Prompt.DeclareAttackers {} -> not (null (answer :: [ObjectId.ObjectId]))
  _ -> False

setUp :: Answerer.Answerer (Asked.Asked GameState.GameState) (State.State Generator.Generator) -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> State.State Generator.Generator GameState.GameState
setUp answerer matchup =
  fmap snd (Engine.runGameAsked (Answerer.unwrap answerer) (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame (Resolve.performHandAction Engine.playSubgame) matchup))

-- Four Grizzly Bears and three Forests apiece: the opening hands empty both
-- libraries, so the game ends at the second turn's draw (CR 704.5b). For a
-- case that needs a result and nothing of the play before it.
sevens :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
sevens s registry = do
  bear <- S.printingOf s registry "Grizzly Bears"
  forest <- S.printingOf s registry "Forest"
  let deck = Deck.fromCards (Map.fromList [(bear, 4), (forest, 3)])
  pure ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])

-- Twelve Grizzly Bears and eight Forests apiece: short enough to deck out.
bears :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
bears s registry = do
  bear <- S.printingOf s registry "Grizzly Bears"
  forest <- S.printingOf s registry "Forest"
  let deck = Deck.fromCards (Map.fromList [(bear, 12), (forest, 8)])
  pure ((S.alice, deck) NonEmpty.:| [(S.bob, deck)])
