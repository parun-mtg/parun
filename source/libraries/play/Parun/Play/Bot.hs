{-# LANGUAGE GADTs #-}

-- | The random bot: a seat that picks among what each prompt offers, each
-- option equally likely, and never reads its view of the game.
module Parun.Play.Bot where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import Numeric.Natural (Natural)
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Damage as Damage
import qualified Parun.Engine.Modal as Modal
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Resolve.Common as Common
import qualified Parun.Engine.Subtype as Subtype
import qualified Parun.Engine.Target as Target
import qualified Parun.Engine.TextWord as TextWord
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Generator as Generator
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.BuybackDecision as BuybackDecision
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.CommandZoneDecision as CommandZoneDecision
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.Dividing as Dividing
import qualified Parun.Types.EntwineDecision as EntwineDecision
import qualified Parun.Types.ForageMode as ForageMode
import qualified Parun.Types.Generator as Generator.Type
import qualified Parun.Types.GraveyardArrangement as GraveyardArrangement
import qualified Parun.Types.GuessAnswer as GuessAnswer
import qualified Parun.Types.KickerDecision as KickerDecision
import qualified Parun.Types.LandOrNonland as LandOrNonland
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.LoopMembers as LoopMembers
import qualified Parun.Types.MoveSpread as MoveSpread
import qualified Parun.Types.MulliganDecision as MulliganDecision
import qualified Parun.Types.MutateSide as MutateSide
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Parity as Parity
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.RollAdjustment as RollAdjustment
import qualified Parun.Types.TargetCount as TargetCount
import qualified Parun.Types.TimeTravelChoice as TimeTravelChoice

-- | Each prompt a bot game over the decks in data/decks reaches, plus
-- ChooseGuess, ChooseNumber, every prompt offering one of a list or a fixed
-- set of decisions or a number up to a bound, every division of counters or
-- an amount, and the prompts sharing an arm's shape, answered with one of its
-- legal answers, each equally likely, an unbounded number capped one past its
-- floor. Never concedes. A card name is any of `names`, the reference's
-- (Parun.Play.Game.bot).
bot :: (Monad m) => [CardName.CardName] -> Answerer.Answerer (Asked.Asked PlayerView.PlayerView) (State.StateT Generator.Type.Generator m)
bot names = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.ChooseAction _ _ actions -> maybe (pure Action.Pass) Generator.pick (NonEmpty.nonEmpty actions)
  Prompt.DeclareMulligan {} -> Generator.pick (MulliganDecision.Keep NonEmpty.:| [MulliganDecision.Mulligan])
  Prompt.Bottom _ _ cards count -> fmap (List.genericTake count) (Generator.shuffle cards)
  Prompt.ChooseDiscard _ _ cards count _ -> fmap (List.genericTake count) (Generator.shuffle cards)
  Prompt.DeclareAttackers _ _ attackers -> Monad.filterM (const coin) attackers
  -- CR 508.1e / 702.22c: any disjoint bands of the attackers; the engine drops
  -- a band the rules do not allow (Parun.Engine.Combat's legalBand).
  Prompt.DeclareBands _ _ attackers -> fmap (List.sort . fmap Set.fromList . filter ((> 1) . length)) (partition attackers)
  Prompt.ChooseAttackTarget _ _ _ targets -> Generator.pick targets
  Prompt.DeclareBlockers _ _ blockers attackers ->
    fmap (Map.fromList . Maybe.catMaybes) . Traversable.for blockers $ \blocker ->
      fmap (fmap (\attacker -> (blocker, Set.singleton attacker))) (Generator.pick (Nothing NonEmpty.:| fmap Just attackers))
  Prompt.ChooseTargets _ _ _ slots -> traverse (\(count, legal) -> fmap (Set.fromList . List.genericTake count) (Generator.shuffle (Set.toList legal))) slots
  Prompt.ChooseManaSource _ _ _ sources -> fmap Just (Generator.pick sources)
  Prompt.ChooseExtraManaSource _ _ sources -> Generator.pick (Nothing NonEmpty.<| fmap Just sources)
  Prompt.ChooseManaToSpend _ _ units -> Generator.pick units
  Prompt.ChooseManaSpentOnX _ _ _ shares -> Generator.pick shares
  Prompt.ChooseX _ _ _ _ least greatest -> Generator.between least greatest
  Prompt.ChooseOptional {} -> exercisesOrDeclines
  Prompt.PayLifeForMana {} -> exercisesOrDeclines
  Prompt.ChooseManaYield _ _ _ options -> Generator.pick options
  Prompt.ChooseTurnMarker _ _ markers -> Generator.pick markers
  Prompt.ChooseCombatDamageOption {} -> exercisesOrDeclines
  Prompt.OrderTriggers _ _ entries -> order entries
  Prompt.OrderTimestamps _ _ objects -> order objects
  Prompt.OrderManaActivations _ _ objects -> order objects
  Prompt.OrderCombatTolls _ _ objects -> order objects
  Prompt.OrderComponentCards _ _ _ printings -> order printings
  Prompt.OrderCostComponents _ _ _ components -> order components
  Prompt.OrderDamage _ _ events -> order events
  Prompt.OrderForEach _ _ _ recipients -> order recipients
  Prompt.ArrangeLibraryCards _ _ objects -> order objects
  Prompt.ArrangeLibraryArrivals _ _ _ objects -> order objects
  -- CR 701.22a / 701.25a / 701.29a: any of the cards to the bottom (the
  -- graveyard, surveilling), the rest on top, each in any order.
  Prompt.ChooseScry _ _ cards -> split cards
  Prompt.ChooseSurveil _ _ cards -> split cards
  Prompt.ChooseFateseal _ _ _ cards -> split cards
  -- CR 700.2: every selection the instruction allows over the legal modes.
  Prompt.ChooseModes _ _ _ legal selection -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) (Modal.selections legal selection)
  -- CR 601.2c: each slot's count from its floor to what the board supplies.
  Prompt.AnnounceTargets _ _ _ offers -> traverse (\(count, legal) -> Generator.between (TargetCount.least count) (TargetCount.ceilingOn (Natural.length legal) count)) offers
  Prompt.ReverseManaAbilities {} -> exercisesOrDeclines
  Prompt.ChooseDamageSource _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseCardInHand _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseColor _ _ _ colors -> Generator.pick colors
  Prompt.ChooseManaType _ _ _ types -> Generator.pick types
  -- CR 205.3: any member of the family, or of the card's list of them.
  Prompt.ChooseSubtype _ _ _ family among -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter (Subtype.offered family among) [minBound ..])
  -- The printed "and/or": any nonempty group of the places.
  Prompt.ChooseSearchZones _ _ places -> someOf places
  Prompt.ChooseCost _ _ _ candidates _ -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) candidates
  Prompt.CallCoin {} -> Generator.pick (CoinFace.Heads NonEmpty.:| [CoinFace.Tails])
  -- CR 615.7: every share of the shield, each event taking at most its own
  -- damage (Parun.Engine.Replacement.askAllocation's gate).
  Prompt.AllocateDamage _ _ events share -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) (allocations (fmap DamageEvent.amount events) share)
  Prompt.ChooseTaps _ _ _ candidates count -> fmap (Set.fromList . List.genericTake count) (Generator.shuffle candidates)
  Prompt.ChooseReturns _ _ _ candidates count -> fmap (Set.fromList . List.genericTake count) (Generator.shuffle candidates)
  Prompt.ChooseExiles _ _ _ candidates count -> fmap (Set.fromList . List.genericTake count) (Generator.shuffle candidates)
  Prompt.ChooseExilesFromGraveyard _ _ _ candidates count -> fmap (Set.fromList . List.genericTake count) (Generator.shuffle candidates)
  -- CR 702.122a / 701.59a: any group whose total power or mana value reaches
  -- the threshold, judged by the values the prompt carries.
  Prompt.ChooseTapsForTotalPower _ _ _ candidates power -> fmap (maybe (Replay.defaultAnswer (Asked.prompt asked)) Set.fromList) (reaching (toInteger power) candidates)
  Prompt.ChooseExilesTotalling _ _ _ _ candidates total -> fmap (maybe (Replay.defaultAnswer (Asked.prompt asked)) Set.fromList) (reaching (toInteger total) candidates)
  -- CR 702.167a: the count exactly, or at least it where "one or more" makes
  -- it a floor.
  Prompt.ChooseMaterials _ _ _ candidates count orMore
    | orMore -> fmap Set.fromList (atLeast count candidates)
    | otherwise -> fmap (Set.fromList . List.genericTake count) (Generator.shuffle candidates)
  -- CR 608.2d / 701.34a: any number of the candidates, each equally likely, or
  -- one of them where one is chosen.
  Prompt.ChooseLoopMembers _ _ _ members candidates -> case (members, NonEmpty.nonEmpty candidates) of
    (LoopMembers.One, Just offered) -> fmap Set.singleton (Generator.pick offered)
    _ -> fmap Set.fromList (Monad.filterM (const coin) candidates)
  Prompt.ChooseProliferate _ _ objects players -> (,) <$> fmap Set.fromList (Monad.filterM (const coin) objects) <*> fmap Set.fromList (Monad.filterM (const coin) players)
  -- CR 701.56a: each candidate left alone, given a time counter, or losing one.
  Prompt.ChooseTimeTravel _ _ _ candidates ->
    fmap (Map.fromList . Maybe.catMaybes) . Traversable.for candidates $ \candidate ->
      fmap (fmap ((,) candidate)) (Generator.pick (Nothing NonEmpty.:| fmap Just [TimeTravelChoice.Add, TimeTravelChoice.Remove]))
  -- CR 400.11c: any count in range, each card any offer; Parun.Engine.Event's
  -- settleOutside drops a name past its copies and tops up to the floor.
  Prompt.ChooseFromOutsideTheGame _ _ candidates least most -> do
    count <- Generator.between least most
    traverse (const (fmap fst (Generator.pick candidates))) (List.genericReplicate count ())
  -- CR 700.3: any pile; any assignment of each creature to one of the piles,
  -- any of them empty (CR 700.3d).
  Prompt.ChoosePile _ _ _ piles _ -> Generator.pick piles
  Prompt.DividePiles _ _ creatures count -> do
    assigned <- traverse (\creature -> fmap (\pile -> (pile, creature)) (Generator.between 0 (count - 1))) (if count == 0 then [] else creatures)
    pure (fmap (\pile -> Set.fromList [creature | (at, creature) <- assigned, at == pile]) (List.genericTake count [0 ..]))
  -- CR 404.3: the cards from the top down in any order.
  Prompt.ArrangeGraveyardArrivals _ _ cards -> fmap GraveyardArrangement.InOrder (order cards)
  -- CR 702.47a/b: any of the offered cards, in any order.
  Prompt.ChooseSplice _ _ _ offers -> arrangement (fmap fst offers)
  -- Any group of the players, each taking any member's total;
  -- Parun.Engine.Resolve.Arms.Zones refuses what CR 119.7-8 or 810.9f forbid.
  Prompt.ChooseRedistribution _ _ offered -> do
    takers <- Monad.filterM (const coin) (fmap fst offered)
    fmap (Map.fromList . zip takers) (Generator.shuffle takers)
  Prompt.OfferedCast {} -> exercisesOrDeclines
  -- Each of these is a "may" or an either-or, both answers always legal.
  Prompt.ChooseCommandZoneOfferFirst {} -> exercisesOrDeclines
  Prompt.ChooseDredge {} -> exercisesOrDeclines
  Prompt.ChooseExert {} -> exercisesOrDeclines
  Prompt.ChooseExplore {} -> exercisesOrDeclines
  Prompt.ChoosePayLifeOnEntry {} -> exercisesOrDeclines
  Prompt.ChooseRedirect {} -> exercisesOrDeclines
  Prompt.ChooseRepeat {} -> exercisesOrDeclines
  Prompt.ChooseRiot {} -> exercisesOrDeclines
  Prompt.ChooseSubstitute {} -> exercisesOrDeclines
  Prompt.ChooseTribute {} -> exercisesOrDeclines
  Prompt.ChooseTurnUpAttachment {} -> exercisesOrDeclines
  Prompt.ChooseUnleash {} -> exercisesOrDeclines
  Prompt.OfferedExtra {} -> exercisesOrDeclines
  Prompt.OfferedMiracleReveal {} -> exercisesOrDeclines
  Prompt.Rethrow {} -> exercisesOrDeclines
  Prompt.ChooseOfferedCastSpell _ _ options -> Generator.pick options
  -- CR 707.5: any candidate, or the card's "may" declined.
  Prompt.ChooseCopyTarget _ _ _ candidates -> Generator.pick (Nothing NonEmpty.:| fmap Just candidates)
  Prompt.ChooseAnyNumber _ _ _ _ candidates most _ -> fmap Set.fromList (atMost (Maybe.fromMaybe (List.genericLength candidates) most) candidates)
  -- CR 510.1c-e / 702.19b-c: every division whose gates this creature's
  -- damage clears beside what the step's attacking creatures announced so far.
  -- Not implemented: a division leaning on damage a creature announces later
  -- in the same group (#6637).
  Prompt.AssignCombatDamage _ _ _ thresholds paid power ->
    pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter (\division -> Damage.tiersCleared thresholds (Map.unionWith (+) paid division) Set.empty division) (divisions (Map.keys thresholds) power))
  Prompt.Search _ _ cards most -> atMost most cards
  -- CR 601.2d / 115.7f: every division the prompt allows.
  Prompt.ChooseDivision _ _ _ dividing targets ->
    pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter (Target.divides dividing targets) (divisions (NonEmpty.toList targets) (Dividing.total dividing)))
  Prompt.ChooseToPay {} -> Generator.pick (PaymentDecision.Declines NonEmpty.:| [PaymentDecision.Pays])
  Prompt.ChoosePermanent _ _ _ candidates _ -> Generator.pick candidates
  Prompt.ChooseCardFromAmong _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseSacrifices _ _ _ candidates count _ -> fmap (Set.fromList . List.genericTake count) (Generator.shuffle candidates)
  -- CR 118.1 / 601.2h / 608.2d: any bearer, then any division of its
  -- counters the count allows, each within what its permanent carries.
  Prompt.ChooseCounterRemoval _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseCounterRemovalAmong _ _ _ owed offered -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter (Cost.dividesRemoval owed offered) (removals offered))
  Prompt.ChooseCounterRemovalAtLeast _ _ _ least offered -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter (Cost.dividesRemovalAtLeast least offered) (removals offered))
  Prompt.ChooseCounterRemovalUpTo _ _ _ most offered -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter ((<= most) . sum) (removals offered))
  -- CR 122.1 / 601.2h / 608.2d: the same by permanent and kind.
  Prompt.ChooseMixedCounterRemoval _ _ _ spread owed offered ->
    pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter (Cost.dividesMixedRemoval spread owed offered) (fmap Cost.unflattenMixed (removals (Cost.flattenMixed offered))))
  -- CR 608.2d: every division of the amount, each share at least one.
  Prompt.ChooseDistribution _ _ _ total candidates ->
    pickOr (Replay.defaultAnswer (Asked.prompt asked)) (filter (Common.distributes total candidates) (divisions (NonEmpty.toList candidates) total))
  -- CR 122.5 / 706.8b: any count of each kind or result up to its tally, at
  -- least one counter in all where the card says "one or more".
  Prompt.ChooseMovedCounters _ _ _ _ offered -> upTo offered
  Prompt.ChooseMovedCountersAtLeastOne _ _ _ _ offered -> untilSome offered (upTo offered)
  Prompt.ChooseStoredRerolls _ _ _ stored -> upTo stored
  -- CR 122.5: each kind's counters divided among the recipients as the
  -- spread allows.
  Prompt.ChooseDistributedMovedCounters _ _ _ spread offered recipients -> spreadMoved spread offered (NonEmpty.toList recipients)
  -- CR 608.2d: any branch left to choose, or neither where each branch
  -- carries its own decline.
  Prompt.ChooseClause _ _ _ _ branches neither _ -> Generator.pick ((if neither then (Nothing NonEmpty.<|) else id) (fmap Just branches))
  Prompt.ChooseGuess {} -> Generator.pick (GuessAnswer.Holds NonEmpty.:| [GuessAnswer.DoesNotHold])
  -- CR 107.1c: an unbounded number, a bid's included, reaches one past its
  -- floor, so a bid tops the high bid by one or declines.
  Prompt.ChooseNumber _ _ _ least _ greatest -> Generator.between least (Maybe.fromMaybe (least + 1) greatest)
  -- CR 612.2: any two different words of a family the text change names.
  Prompt.ChooseWordSwap _ _ _ _ families forbidden -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) (TextWord.swaps families forbidden)
  -- CR 201.4: any name; CR 201.4a's restriction is judged where the
  -- reference is (Parun.Interpreter.policingCardNames), which asks again.
  Prompt.ChooseCardName {} -> pickOr (Replay.defaultAnswer (Asked.prompt asked)) names
  -- Each offer is what the caller found legal: CR 118.13's payable routes,
  -- every total or half CR 601.2f / 118.7e allows, every candidate.
  Prompt.AnnounceHybridHalf _ _ _ _ offers -> Generator.pick offers
  Prompt.AnnounceHybridPayment _ _ _ _ offers -> Generator.pick offers
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> Generator.pick offers
  Prompt.ChooseActivePlayer _ _ players -> Generator.pick players
  Prompt.ChooseAmass _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseAttachment _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseBehold _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseBlight _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseBolster _ _ _ candidates -> Generator.pick candidates
  Prompt.ChooseCardInGraveyard _ _ _ candidates _ -> Generator.pick candidates
  Prompt.ChooseCardType _ _ _ types -> Generator.pick types
  Prompt.ChooseCoinResult _ _ faces -> Generator.pick faces
  Prompt.ChooseConjuredCard _ _ candidates -> Generator.pick candidates
  Prompt.ChooseCounterKind _ _ _ kinds -> Generator.pick kinds
  Prompt.ChooseDefender _ _ players -> Generator.pick players
  Prompt.ChooseDesignation _ _ _ designations -> Generator.pick designations
  Prompt.ChooseDungeon _ _ dungeons -> Generator.pick dungeons
  Prompt.ChooseHalf _ _ _ halves -> Generator.pick halves
  Prompt.ChooseWord _ _ _ offered -> Generator.pick offered
  Prompt.ChooseLegend _ _ candidates -> Generator.pick candidates
  Prompt.ChooseNamePosition _ _ _ positions -> Generator.pick positions
  Prompt.ChooseOpponent _ _ _ _ players -> Generator.pick players
  Prompt.ChoosePlayPermission _ _ _ permissions -> Generator.pick permissions
  Prompt.ChooseHeldPermission _ _ _ permissions -> Generator.pick permissions
  Prompt.ChoosePlayer _ _ _ players -> Generator.pick players
  Prompt.ChooseProtector _ _ _ players -> Generator.pick players
  Prompt.ChooseReducedCost _ _ _ totals -> Generator.pick totals
  Prompt.ChooseReductionHalf _ _ _ _ halves -> Generator.pick halves
  Prompt.ChooseRingBearer _ _ candidates -> Generator.pick candidates
  Prompt.ChooseRoom _ _ _ rooms -> Generator.pick rooms
  Prompt.ChooseSticker _ _ _ stickers -> Generator.pick stickers
  Prompt.ChooseVote _ _ _ choices abstain -> Generator.pick ((if abstain then (Nothing NonEmpty.<|) else id) (fmap Just choices))
  -- Any index into the offer.
  Prompt.ChooseDelayedTriggerEvent _ _ _ events -> indexInto (Asked.prompt asked) (NonEmpty.toList events)
  Prompt.ChooseDieResult _ _ _ results -> indexInto (Asked.prompt asked) (NonEmpty.toList results)
  Prompt.ChooseEntryOption _ _ _ options -> indexInto (Asked.prompt asked) options
  Prompt.ChooseReplacement _ _ entries -> indexInto (Asked.prompt asked) entries
  Prompt.ChooseRollModifier _ _ modifiers -> indexInto (Asked.prompt asked) (NonEmpty.toList modifiers)
  Prompt.ChooseStickerKeeper _ _ printings -> indexInto (Asked.prompt asked) (NonEmpty.toList printings)
  Prompt.KeepHand _ _ hands -> indexInto (Asked.prompt asked) (NonEmpty.toList hands)
  -- Any of the offer, or none: each a "may", an "up to one" or a "wish to".
  Prompt.CastWhileSearching _ _ offers -> orNone offers
  Prompt.ChooseAssistant _ _ _ players -> orNone (NonEmpty.toList players)
  Prompt.ChooseCompanion _ _ companions -> orNone (NonEmpty.toList companions)
  Prompt.ChooseEncode _ _ _ candidates -> orNone (NonEmpty.toList candidates)
  Prompt.ChooseEnlist _ _ _ candidates -> orNone (NonEmpty.toList candidates)
  Prompt.ChooseLearn _ _ _ modes -> orNone (NonEmpty.toList modes)
  Prompt.ChooseMovedCounterOrNone _ _ _ _ kinds -> orNone (NonEmpty.toList kinds)
  Prompt.ChooseRevealOnEntry _ _ _ candidates -> orNone (NonEmpty.toList candidates)
  Prompt.ChooseStickerOrNone _ _ _ stickers -> orNone (NonEmpty.toList stickers)
  Prompt.MulliganAction _ _ actions -> orNone actions
  Prompt.OpeningHandAction _ _ actions -> orNone actions
  -- Each of these is a decision both of whose answers the prompt allows: an
  -- optional cost asked only where it is payable (Parun.Engine.Cast), forage
  -- only where both halves can be carried out.
  Prompt.ChooseBuyback {} -> Generator.pick (BuybackDecision.Declines NonEmpty.:| [BuybackDecision.BuysBack])
  Prompt.ChooseEntwine {} -> Generator.pick (EntwineDecision.Declines NonEmpty.:| [EntwineDecision.Entwines])
  Prompt.ChooseForage {} -> Generator.pick (ForageMode.ExileCards NonEmpty.:| [ForageMode.SacrificeFood])
  Prompt.ChooseParity {} -> Generator.pick (Parity.Odd NonEmpty.:| [Parity.Even])
  Prompt.ChooseMutateSide {} -> Generator.pick (MutateSide.Over NonEmpty.:| [MutateSide.Under])
  Prompt.ChooseLibraryEnd {} -> Generator.pick (LibraryPosition.Top NonEmpty.:| [LibraryPosition.Bottom])
  Prompt.ChooseClash {} -> Generator.pick (LibraryPosition.Top NonEmpty.:| [LibraryPosition.Bottom])
  Prompt.ReturnCommander {} -> Generator.pick (CommandZoneDecision.Leaves NonEmpty.:| [CommandZoneDecision.Returns])
  -- CR 614.1a: either kind, or the draw left standing.
  Prompt.ChooseLandOrNonland {} -> orNone [LandOrNonland.Land, LandOrNonland.Nonland]
  -- CR 601.2b / 702.33c: any count up to the limit, "any number of times"
  -- capped one past declining; one more is payable wherever it is asked.
  Prompt.ChooseKicker _ _ _ _ _ limit -> fmap KickerDecision.MkKickerDecision (Generator.between 0 (Maybe.fromMaybe 1 limit))
  -- A number up to the bound, from zero (CR 118.3's payment) or the first
  -- chapter (CR 702.155b).
  Prompt.ChooseAssistAmount _ _ _ most -> Generator.between 0 most
  Prompt.ChoosePaidEnergy _ _ _ most -> Generator.between 0 most
  Prompt.ChooseReadAheadChapter _ _ _ final -> Generator.between 1 final
  -- CR 706.2b: any die moved either way, or the offer declined; raised only
  -- once its cost is payable.
  Prompt.AdjustDieRoll _ _ results _ _ -> orNone [(index, adjustment) | index <- zipWith const [0 ..] (NonEmpty.toList results), adjustment <- [RollAdjustment.Increase, RollAdjustment.Decrease]]
  prompt -> pure (Replay.defaultAnswer prompt)

-- | Any index into the items, each equally likely, or the prompt's default
-- where there is none.
indexInto :: (Monad m) => Prompt.Prompt Natural -> [a] -> State.StateT Generator.Type.Generator m Natural
indexInto prompt items = pickOr (Replay.defaultAnswer prompt) (zipWith const [0 ..] items)

-- | Any of the items or none, each equally likely.
orNone :: (Monad m) => [a] -> State.StateT Generator.Type.Generator m (Maybe a)
orNone items = Generator.pick (Nothing NonEmpty.:| fmap Just items)

-- | Any of the options, each equally likely, or the fallback where there is
-- none.
pickOr :: (Monad m) => a -> [a] -> State.StateT Generator.Type.Generator m a
pickOr fallback = maybe (pure fallback) Generator.pick . NonEmpty.nonEmpty

-- | Every division of the offered counters, each permanent giving from none
-- to all it carries, zero shares left out.
removals :: (Ord k) => Map.Map k Natural -> [Map.Map k Natural]
removals offered = fmap (Map.filter (> 0) . Map.fromList) (traverse (\(key, carried) -> fmap ((,) key) [0 .. carried]) (Map.toList offered))

-- | Any count of each key from none to its tally, each such answer equally
-- likely, zero counts left out.
upTo :: (Monad m) => Map.Map k Natural -> State.StateT Generator.Type.Generator m (Map.Map k Natural)
upTo = fmap (Map.filter (> 0)) . traverse (Generator.between 0)

-- | The draw again until it names something, so each nonempty answer stays
-- equally likely; empty only when the tallies offer nothing.
untilSome :: (Monad m) => Map.Map k Natural -> State.StateT Generator.Type.Generator m (Map.Map r v) -> State.StateT Generator.Type.Generator m (Map.Map r v)
untilSome offered draw
  | all (== 0) offered = pure Map.empty
  | otherwise = do
      answer <- draw
      if Map.null answer then untilSome offered draw else pure answer

-- | CR 122.5: a move's counters divided among the recipients, each answer the
-- spread allows equally likely: each kind's tally up to all of it, at least
-- one counter in all, all of it, one kind's count of it, or at most one
-- counter in all (Parun.Types.MoveSpread).
spreadMoved :: (Monad m, Ord k, Ord r) => MoveSpread.MoveSpread -> Map.Map k Natural -> [r] -> State.StateT Generator.Type.Generator m (Map.Map r (Map.Map k Natural))
spreadMoved spread offered recipients = case spread of
  MoveSpread.AnyNumber -> perKind (\held -> concatMap (divisions recipients) [0 .. held])
  MoveSpread.AtLeastOne -> untilSome offered (perKind (\held -> concatMap (divisions recipients) [0 .. held]))
  MoveSpread.Exactly -> perKind (divisions recipients)
  MoveSpread.OneKind count -> pickOr Map.empty [byRecipient (Map.singleton kind division) | (kind, held) <- Map.toList offered, division <- divisions recipients (min count held)]
  MoveSpread.UpToOne -> fmap (maybe Map.empty (\(recipient, kind) -> Map.singleton recipient (Map.singleton kind 1))) (orNone [(recipient, kind) | recipient <- recipients, (kind, held) <- Map.toList offered, held > 0])
  where
    perKind shares = fmap byRecipient (traverse (pickOr Map.empty . shares) offered)
    byRecipient divided = Map.unionsWith Map.union (fmap (\(kind, division) -> fmap (Map.singleton kind) division) (Map.toList divided))

-- | CR 103.1: whether the bot, picked to choose who plays first, plays first
-- rather than draws, each equally likely.
playsFirst :: (Monad m) => State.StateT Generator.Type.Generator m Bool
playsFirst = coin

-- | A subset of at most so many of the items, each such subset equally
-- likely: its size drawn by how many subsets have it, then its members.
atMost :: (Monad m) => Natural -> [a] -> State.StateT Generator.Type.Generator m [a]
atMost most items = do
  let total = List.genericLength items :: Natural
  size <- weighted (fmap (\size -> (size, choose total size)) [0 .. min most total])
  fmap (List.genericTake size) (Generator.shuffle items)

-- | A subset of at least so many of the items, each such subset equally
-- likely, or all of them where there are fewer: atMost's draw over the sizes
-- from the floor up.
atLeast :: (Monad m) => Natural -> [a] -> State.StateT Generator.Type.Generator m [a]
atLeast least items = do
  let total = List.genericLength items :: Natural
  size <- weighted (fmap (\size -> (size, choose total size)) [min least total .. total])
  fmap (List.genericTake size) (Generator.shuffle items)

-- | A subset of the candidates whose values sum to at least the threshold,
-- each such subset equally likely, or Nothing where none does: each candidate
-- kept with the share of the reaching subsets left that keep it.
reaching :: (Monad m) => Integer -> [(a, Integer)] -> State.StateT Generator.Type.Generator m (Maybe [a])
reaching threshold candidates
  | NonEmpty.head counts threshold == 0 = pure Nothing
  | otherwise = fmap Just (walk threshold (zip candidates (NonEmpty.tail counts)))
  where
    counts = reachCounts (fmap snd candidates)
    walk owed pending = case pending of
      [] -> pure []
      ((candidate, value), later) : rest -> do
        let kept = later (owed - value)
        draw <- Generator.between 0 (kept + later owed - 1)
        if draw < kept then fmap (candidate :) (walk (owed - value) rest) else walk owed rest

-- | For each suffix of the values, the whole list first and the empty one
-- last, how many of its subsets sum to at least a total.
reachCounts :: [Integer] -> NonEmpty.NonEmpty (Integer -> Natural)
reachCounts values = case values of
  [] -> pure (\total -> if total <= 0 then 1 else 0)
  value : rest ->
    let later@(next NonEmpty.:| _) = reachCounts rest
        least = sum (filter (< 0) values)
        table = Map.fromList [(total, next total + next (total - value)) | total <- [least + 1 .. sum (filter (> 0) values)]]
        count total
          | total <= least = 2 ^ List.length values
          | otherwise = Map.findWithDefault 0 total table
     in count NonEmpty.<| later

-- | Any of the items in any order, each such sequence equally likely: its
-- length drawn by how many sequences have it, then its members.
arrangement :: (Monad m) => [a] -> State.StateT Generator.Type.Generator m [a]
arrangement items = do
  let total = List.genericLength items :: Natural
  size <- weighted (fmap (\size -> (size, product [total - size + 1 .. total])) [0 .. total])
  fmap (List.genericTake size) (Generator.shuffle items)

-- | A partition of the items into nonempty blocks, each partition equally
-- likely: the first item's companions drawn by how many partitions they
-- leave, then the rest partitioned alike.
partition :: (Monad m) => [a] -> State.StateT Generator.Type.Generator m [[a]]
partition items = case items of
  [] -> pure []
  first : rest -> do
    let others = List.genericLength rest :: Natural
    companions <- weighted (fmap (\k -> (k, choose others k * bell (others - k))) [0 .. others])
    shuffled <- Generator.shuffle rest
    let (block, remaining) = List.genericSplitAt companions shuffled
    fmap ((first : block) :) (partition remaining)

-- | One of the values, each as likely as its weight, which are not all zero.
weighted :: (Monad m) => [(Natural, Natural)] -> State.StateT Generator.Type.Generator m Natural
weighted options = do
  draw <- Generator.between 0 (sum (fmap snd options) - 1)
  pure (maybe 0 fst (List.find ((> draw) . snd) (zip (fmap fst options) (scanl1 (+) (fmap snd options)))))

-- | How many ways to choose k of n.
choose :: Natural -> Natural -> Natural
choose n k = product [n - k + 1 .. n] `div` product [1 .. k]

-- | How many partitions a set of n items has.
bell :: Natural -> Natural
bell = List.genericIndex bells
  where
    bells = 1 : fmap (\n -> sum (zipWith (\k b -> choose n k * b) [0 .. n] bells)) [0 ..]

-- | A permutation of the items' indices, each equally likely.
order :: (Monad m) => [a] -> State.StateT Generator.Type.Generator m [Natural]
order items = Generator.shuffle (zipWith const [0 ..] items)

-- | The items in any order, cut at any point, each such answer equally likely.
split :: (Monad m) => [a] -> State.StateT Generator.Type.Generator m ([a], [a])
split items = do
  shuffled <- Generator.shuffle items
  at <- Generator.between 0 (List.genericLength items)
  pure (List.genericSplitAt at shuffled)

-- | A nonempty subset of the items, each equally likely; empty only when the
-- items are.
someOf :: (Monad m, Ord a) => Set.Set a -> State.StateT Generator.Type.Generator m (Set.Set a)
someOf items = do
  chosen <- fmap Set.fromList (Monad.filterM (const coin) (Set.toList items))
  if Set.null chosen && not (Set.null items) then someOf items else pure chosen

-- | Every way to spend the whole amount over the caps, each share within its
-- own.
allocations :: [Natural] -> Natural -> [[Natural]]
allocations caps amount = case caps of
  [] -> [[] | amount == 0]
  cap : rest -> do
    share <- [0 .. min cap amount]
    fmap (share :) (allocations rest (amount - share))

-- | Every way to divide an amount among the keys, zero shares left out.
divisions :: (Ord k) => [k] -> Natural -> [Map.Map k Natural]
divisions keys amount = case keys of
  [] -> [Map.empty | amount == 0]
  key : rest -> do
    share <- [0 .. amount]
    fmap (if share == 0 then id else Map.insert key share) (divisions rest (amount - share))

-- | A "may" taken or declined, each equally likely.
exercisesOrDeclines :: (Monad m) => State.StateT Generator.Type.Generator m OptionalDecision.OptionalDecision
exercisesOrDeclines = Generator.pick (OptionalDecision.Exercises NonEmpty.:| [OptionalDecision.Declines])

coin :: (Monad m) => State.StateT Generator.Type.Generator m Bool
coin = Generator.pick (True NonEmpty.:| [False])
