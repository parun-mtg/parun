{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- The one loop every answerer plugs into: a prompt is routed to the
-- answerer for its seat, observers watch it either side of the answer, and a
-- game is stepped until it ends or its caller says stop.
module Parun.Engine.Driver where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Numeric.Natural (Natural)
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GraveyardOrder as GraveyardOrder
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView.Type
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Result as Result

-- | A prompt whose seat has an answerer goes to it; every other prompt, chance
-- included, goes to the fallback.
bySeat :: Map.Map PlayerId.PlayerId (Answerer.Answerer (Asked.Asked GameState.GameState) m) -> Answerer.Answerer (Asked.Asked GameState.GameState) m -> Answerer.Answerer (Asked.Asked GameState.GameState) m
bySeat seats fallback = Answerer.MkAnswerer $ \asked ->
  let answerer = Maybe.fromMaybe fallback (deciderOf (Asked.prompt asked) >>= (`Map.lookup` seats))
   in Answerer.unwrap answerer asked

-- | CR 400.2 / 724.4: a prompt some seat decides goes to `answerer` as that
-- seat may see it, never the whole game; every other prompt, chance
-- included, goes to the fallback. Parun.PlayerViewSpec's "CR 400.2 a seat
-- answers from its own view" is the proof.
seated :: Answerer.Answerer (Asked.Asked PlayerView.Type.PlayerView) m -> Answerer.Answerer (Asked.Asked GameState.GameState) m -> Answerer.Answerer (Asked.Asked GameState.GameState) m
seated answerer fallback = Answerer.MkAnswerer $ \asked -> case deciderOf (Asked.prompt asked) of
  Just seat -> Answerer.unwrap answerer (Asked.mapGames (PlayerView.viewFor seat) asked)
  Nothing -> Answerer.unwrap fallback asked

-- | Every observer's before, in order, then the answer, then every observer's
-- after, in order.
ask :: (Monad m) => [Observer.Observer m] -> Answerer.Answerer (Asked.Asked GameState.GameState) m -> Asked.Asked GameState.GameState r -> m r
ask observers answerer asked = do
  mapM_ (\observer -> Observer.before observer asked) observers
  answer <- Answerer.unwrap answerer asked
  mapM_ (\observer -> Observer.after observer asked answer) observers
  pure answer

-- | A whole game, from setup to its result, each player in `orders` starting
-- at that graveyard order (CR 404.3) and every other at Indifferent.
match :: (Monad m) => [Observer.Observer m] -> Answerer.Answerer (Asked.Asked GameState.GameState) m -> GameSettings.GameSettings -> Map.Map PlayerId.PlayerId GraveyardOrder.GraveyardOrder -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> m (Result.Result, GameState.GameState)
match observers answerer settings orders matchup =
  Engine.runGameAsked (ask observers answerer) (Map.foldrWithKey Game.setGraveyardOrder (Setup.gameWith settings (fmap fst matchup)) orders) (Engine.playFrom matchup)

-- | Whole steps (CR 500.1) from a state, until the game has a result, the
-- budget is spent, or the predicate holds. In Grand Melee the budget counts
-- Engine.runStep's units instead (CR 807.4b).
drive :: (Monad m) => [Observer.Observer m] -> Answerer.Answerer (Asked.Asked GameState.GameState) m -> Natural -> (GameState.GameState -> m Bool) -> GameState.GameState -> m GameState.GameState
drive observers answerer budget stop gs = do
  stopping <- stop gs
  if budget == 0 || Maybe.isJust (GameState.result gs) || stopping
    then pure gs
    else do
      (_, next) <- Engine.runGameAsked (ask observers answerer) gs Engine.runStep
      drive observers answerer (budget - 1) stop next

-- | The player a prompt's choice is for, whom deciderOf may differ from: a
-- player controlling them (CR 724.1) or a chooser an effect names (Odric,
-- Master Tactician); Nothing for a prompt no seat answers and for Concede,
-- which a player always answers themselves (CR 724.6).
subjectOf :: Prompt.Prompt r -> Maybe PlayerId.PlayerId
subjectOf prompt = case prompt of
  Prompt.ChooseAction _ pid _ -> Just pid
  Prompt.Concede {} -> Nothing
  Prompt.ChooseTurnMarker _ pid _ -> Just pid
  Prompt.Shuffle {} -> Nothing
  Prompt.RandomFirstPlayer {} -> Nothing
  Prompt.RandomObject {} -> Nothing
  Prompt.RandomStickerSheet {} -> Nothing
  Prompt.RandomCard {} -> Nothing
  Prompt.ChooseConjuredCard _ pid _ -> Just pid
  Prompt.RandomDepth {} -> Nothing
  Prompt.RandomPlayer {} -> Nothing
  Prompt.RollDie {} -> Nothing
  Prompt.LookUpCard {} -> Nothing
  Prompt.ReferenceCards {} -> Nothing
  Prompt.ReferenceNames {} -> Nothing
  Prompt.ChooseDieResult _ pid _ _ -> Just pid
  Prompt.ChooseStoredRerolls _ pid _ _ -> Just pid
  Prompt.Rethrow _ pid _ _ -> Just pid
  Prompt.RerollDice _ pid _ _ -> Just pid
  Prompt.AdjustDieRoll _ pid _ _ _ -> Just pid
  Prompt.ChooseRollModifier _ pid _ -> Just pid
  Prompt.FlipCoin {} -> Nothing
  Prompt.CallCoin _ pid -> Just pid
  Prompt.ChooseCoinResult _ pid _ -> Just pid
  Prompt.ChooseDiscard _ pid _ _ _ -> Just pid
  Prompt.ChooseScry _ pid _ -> Just pid
  Prompt.ChooseSurveil _ pid _ -> Just pid
  Prompt.ChooseFateseal _ pid _ _ -> Just pid
  Prompt.ChooseExplore _ pid _ _ -> Just pid
  Prompt.ChooseDefender _ pid _ -> Just pid
  Prompt.ChooseManaSource _ pid _ _ -> Just pid
  Prompt.ChooseExtraManaSource _ pid _ -> Just pid
  Prompt.ReverseManaAbilities _ pid _ -> Just pid
  Prompt.PayLifeForMana _ pid _ _ -> Just pid
  Prompt.ChooseManaYield _ pid _ _ -> Just pid
  Prompt.ChooseManaToSpend _ pid _ -> Just pid
  Prompt.ChooseManaSpentOnX _ pid _ _ -> Just pid
  Prompt.ChooseProliferate _ pid _ _ -> Just pid
  Prompt.ChooseRedistribution _ pid _ -> Just pid
  Prompt.ChooseRingBearer _ pid _ -> Just pid
  Prompt.ChooseBolster _ pid _ _ -> Just pid
  Prompt.ChooseAmass _ pid _ _ -> Just pid
  Prompt.ChooseBlight _ pid _ _ -> Just pid
  Prompt.ChooseBehold _ pid _ _ -> Just pid
  Prompt.ChooseVote _ pid _ _ _ -> Just pid
  Prompt.ChooseCounterKind _ pid _ _ -> Just pid
  Prompt.ChooseMovedCounters _ pid _ _ _ -> Just pid
  Prompt.ChooseMovedCountersAtLeastOne _ pid _ _ _ -> Just pid
  Prompt.ChooseDistributedMovedCounters _ pid _ _ _ _ -> Just pid
  Prompt.ChooseMovedCounterOrNone _ pid _ _ _ -> Just pid
  Prompt.ChoosePaidEnergy _ pid _ _ -> Just pid
  Prompt.ChooseNumber _ pid _ _ _ _ -> Just pid
  Prompt.ChooseReadAheadChapter _ pid _ _ -> Just pid
  Prompt.ChooseDamageSource _ pid _ _ -> Just pid
  Prompt.ChooseDelayedTriggerEvent _ pid _ _ -> Just pid
  Prompt.ChooseCardInGraveyard _ pid _ _ _ -> Just pid
  Prompt.ChooseCardInHand _ pid _ _ -> Just pid
  Prompt.ChooseCardFromAmong _ pid _ _ -> Just pid
  Prompt.ChooseDungeon _ pid _ -> Just pid
  Prompt.ChooseSticker _ pid _ _ -> Just pid
  Prompt.ChooseStickerOrNone _ pid _ _ -> Just pid
  Prompt.ChooseNamePosition _ pid _ _ -> Just pid
  Prompt.ChooseCompanion _ pid _ -> Just pid
  Prompt.ChooseFromOutsideTheGame _ pid _ _ _ -> Just pid
  Prompt.ChooseRoom _ pid _ _ -> Just pid
  Prompt.ChooseHalf _ pid _ _ -> Just pid
  Prompt.ChooseLegend _ pid _ -> Just pid
  Prompt.DeclareAttackers _ pid _ -> Just pid
  Prompt.ChooseAttackTarget _ pid _ _ -> Just pid
  Prompt.DeclareBands _ pid _ -> Just pid
  Prompt.ChooseExert _ pid _ -> Just pid
  Prompt.ChooseEnlist _ pid _ _ -> Just pid
  Prompt.ChooseRerolledDie _ pid _ _ -> Just pid
  Prompt.ChooseEncode _ pid _ _ -> Just pid
  Prompt.DeclareBlockers _ pid _ _ -> Just pid
  Prompt.DividePiles _ pid _ _ -> Just pid
  Prompt.AssignCombatDamage _ pid _ _ _ _ -> Just pid
  Prompt.ChooseCombatDamageOption _ pid _ _ -> Just pid
  Prompt.ChooseTargets _ pid _ _ -> Just pid
  Prompt.AnnounceTargets _ pid _ _ -> Just pid
  Prompt.ChooseWordSwap _ pid _ _ _ _ -> Just pid
  Prompt.ChooseSubtype _ pid _ _ _ -> Just pid
  Prompt.ChooseParity _ pid _ -> Just pid
  Prompt.ChooseWord _ pid _ _ -> Just pid
  Prompt.ChooseSearchZones _ pid _ -> Just pid
  Prompt.Search _ pid _ _ -> Just pid
  Prompt.CastWhileSearching _ pid _ -> Just pid
  Prompt.ChooseX _ pid _ _ _ _ -> Just pid
  Prompt.ChooseEntwine _ pid _ _ -> Just pid
  Prompt.ChooseMutateSide _ pid _ _ -> Just pid
  Prompt.ChooseForage _ pid _ -> Just pid
  Prompt.ChooseLearn _ pid _ _ -> Just pid
  Prompt.ChooseTimeTravel _ pid _ _ -> Just pid
  Prompt.ChooseClash _ pid _ _ _ _ -> Just pid
  Prompt.ChooseKicker _ pid _ _ _ _ -> Just pid
  Prompt.ChooseBuyback _ pid _ _ -> Just pid
  Prompt.ChooseSplice _ pid _ _ -> Just pid
  Prompt.ChooseAssistant _ pid _ _ -> Just pid
  Prompt.ChooseAssistAmount _ pid _ _ -> Just pid
  Prompt.ReturnCommander _ pid _ -> Just pid
  Prompt.ChooseCommandZoneOfferFirst _ pid _ -> Just pid
  Prompt.ChooseLibraryEnd _ pid _ _ -> Just pid
  Prompt.ArrangeLibraryArrivals _ pid _ _ -> Just pid
  Prompt.ArrangeLibraryCards _ pid _ -> Just pid
  Prompt.ArrangeGraveyardArrivals _ pid _ -> Just pid
  Prompt.ChooseModes _ pid _ _ _ -> Just pid
  Prompt.ChooseCopyTarget _ pid _ _ -> Just pid
  Prompt.ChooseEntryOption _ pid _ _ -> Just pid
  Prompt.ChooseRiot _ pid _ -> Just pid
  Prompt.ChooseUnleash _ pid _ -> Just pid
  Prompt.ChooseTribute _ pid _ _ -> Just pid
  Prompt.ChoosePayLifeOnEntry _ pid _ _ -> Just pid
  Prompt.ChooseRevealOnEntry _ pid _ _ -> Just pid
  Prompt.ChooseColor _ pid _ _ -> Just pid
  Prompt.ChooseDesignation _ pid _ _ -> Just pid
  Prompt.ChooseManaType _ pid _ _ -> Just pid
  Prompt.ChooseCardType _ pid _ _ -> Just pid
  Prompt.ChooseCardName _ pid _ _ _ -> Just pid
  Prompt.ChooseGuess _ pid _ _ -> Just pid
  Prompt.ChooseOpponent _ pid _ _ _ -> Just pid
  Prompt.ChooseProtector _ pid _ _ -> Just pid
  Prompt.ChoosePlayer _ pid _ _ -> Just pid
  Prompt.ChooseActivePlayer _ pid _ -> Just pid
  Prompt.OrderTriggers _ pid _ -> Just pid
  Prompt.OrderDamage _ pid _ -> Just pid
  Prompt.AllocateDamage _ pid _ _ -> Just pid
  Prompt.ChooseReplacement _ pid _ -> Just pid
  Prompt.ChooseDredge _ pid _ _ -> Just pid
  Prompt.ChooseLandOrNonland _ pid _ -> Just pid
  Prompt.ChooseRedirect _ pid _ _ -> Just pid
  Prompt.ChooseSubstitute _ pid _ -> Just pid
  Prompt.ChooseSacrifices _ pid _ _ _ _ -> Just pid
  Prompt.ChooseExilesFromGraveyard _ pid _ _ _ -> Just pid
  Prompt.ChooseMaterials _ pid _ _ _ _ -> Just pid
  Prompt.ChooseExilesTotalling _ pid _ _ _ _ _ _ -> Just pid
  Prompt.ChooseAnyNumber _ pid _ _ _ _ _ -> Just pid
  Prompt.ChoosePile _ pid _ _ _ -> Just pid
  Prompt.ChoosePermanent _ pid _ _ _ _ -> Just pid
  Prompt.ChooseTapsForTotalPower _ pid _ _ _ -> Just pid
  Prompt.ChooseTaps _ pid _ _ _ _ -> Just pid
  Prompt.ChooseReturns _ pid _ _ _ -> Just pid
  Prompt.ChooseExiles _ pid _ _ _ -> Just pid
  Prompt.ChooseCounterRemoval _ pid _ _ -> Just pid
  Prompt.ChooseCounterRemovalAmong _ pid _ _ _ -> Just pid
  Prompt.ChooseCounterRemovalAtLeast _ pid _ _ _ -> Just pid
  Prompt.ChooseCounterRemovalUpTo _ pid _ _ _ -> Just pid
  Prompt.ChooseDistribution _ pid _ _ _ -> Just pid
  Prompt.ChooseDivision _ pid _ _ _ -> Just pid
  Prompt.ChooseMixedCounterRemoval _ pid _ _ _ _ -> Just pid
  Prompt.ChooseAttachment _ pid _ _ -> Just pid
  Prompt.ChooseTurnUpAttachment _ pid _ -> Just pid
  Prompt.ChooseCost _ pid _ _ _ -> Just pid
  Prompt.ChoosePlayPermission _ pid _ _ -> Just pid
  Prompt.ChooseHeldPermission _ pid _ _ -> Just pid
  Prompt.OrderCostComponents _ pid _ _ -> Just pid
  Prompt.OrderCombatTolls _ pid _ -> Just pid
  Prompt.OrderComponentCards _ pid _ _ -> Just pid
  Prompt.ChooseStickerKeeper _ pid _ -> Just pid
  Prompt.ChooseTokenKind _ pid _ _ -> Just pid
  Prompt.OrderForEach _ pid _ _ -> Just pid
  Prompt.ChooseLoopMembers _ pid _ _ _ -> Just pid
  Prompt.ChooseRepeat _ pid _ _ -> Just pid
  Prompt.OrderTimestamps _ pid _ -> Just pid
  Prompt.OrderManaActivations _ pid _ -> Just pid
  Prompt.KeepHand _ pid _ -> Just pid
  Prompt.DeclareMulligan _ pid _ -> Just pid
  Prompt.Bottom _ pid _ _ -> Just pid
  Prompt.MulliganAction _ pid _ -> Just pid
  Prompt.OpeningHandAction _ pid _ -> Just pid
  Prompt.ChooseOptional _ pid _ _ _ _ -> Just pid
  Prompt.ChooseClause _ pid _ _ _ _ _ -> Just pid
  Prompt.OfferedCast _ pid _ _ -> Just pid
  Prompt.ChooseOfferedCastSpell _ pid _ -> Just pid
  Prompt.OfferedMiracleReveal _ pid _ _ _ -> Just pid
  Prompt.OfferedExtra _ pid _ _ -> Just pid
  Prompt.ChooseToPay _ pid _ _ _ _ -> Just pid
  Prompt.AnnouncePhyrexianPayment _ pid _ _ _ -> Just pid
  Prompt.AnnounceHybridPayment _ pid _ _ _ -> Just pid
  Prompt.AnnounceHybridHalf _ pid _ _ _ -> Just pid
  Prompt.ChooseHybridHalf _ pid _ _ _ -> Just pid
  Prompt.ChooseReducedCost _ pid _ _ -> Just pid

-- WHO ANSWERS a prompt: the Decider it carries, which CR 724.5 makes the seat
-- that makes every choice a controlled player would make. Not the PlayerId the
-- prompt is about; the two differ only while a CR 724.1 or CR 724.2 effect is
-- running. Prompt.Concede is the exception CR 724.6 names and answers with its
-- own seat, and the random and shuffle prompts answer Nothing -- randomness is
-- not a choice, so no seat makes it.
--
-- Total on purpose: a hand-kept catalog over a GADT this size drifts silently,
-- and the wildcard that would let it drift is what -Wincomplete-patterns is here
-- to refuse.
deciderOf :: Prompt.Prompt r -> Maybe PlayerId.PlayerId
deciderOf prompt = case prompt of
  Prompt.ChooseAction decider _ _ -> Just (Decider.unwrap decider)
  Prompt.Concede pid -> Just pid
  Prompt.ChooseTurnMarker decider _ _ -> Just (Decider.unwrap decider)
  Prompt.Shuffle {} -> Nothing
  Prompt.RandomFirstPlayer {} -> Nothing
  Prompt.RandomObject {} -> Nothing
  Prompt.RandomStickerSheet {} -> Nothing
  Prompt.RandomPlayer {} -> Nothing
  Prompt.RandomCard {} -> Nothing
  Prompt.ChooseConjuredCard decider _ _ -> Just (Decider.unwrap decider)
  Prompt.RollDie {} -> Nothing
  Prompt.RandomDepth {} -> Nothing
  Prompt.LookUpCard {} -> Nothing
  Prompt.ReferenceCards {} -> Nothing
  Prompt.ReferenceNames {} -> Nothing
  Prompt.ChooseDieResult decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.FlipCoin {} -> Nothing
  Prompt.CallCoin decider _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCoinResult decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDiscard decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseScry decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSurveil decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseFateseal decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExplore decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.Rethrow decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.RerollDice decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AdjustDieRoll decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRollModifier decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDefender decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaSource decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExtraManaSource decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ReverseManaAbilities decider _ _ -> Just (Decider.unwrap decider)
  Prompt.PayLifeForMana decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaYield decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaToSpend decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaSpentOnX decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseProliferate decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRedistribution decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRingBearer decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBolster decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAmass decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBlight decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBehold decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemoval decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemovalAmong decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemovalAtLeast decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterRemovalUpTo decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDistribution decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDivision decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMixedCounterRemoval decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseVote decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCounterKind decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMovedCounters decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseStoredRerolls decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMovedCountersAtLeastOne decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDistributedMovedCounters decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMovedCounterOrNone decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePaidEnergy decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseNumber decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReadAheadChapter decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDamageSource decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDelayedTriggerEvent decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardInGraveyard decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardInHand decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardFromAmong decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAnyNumber decider _ _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDungeon decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSticker decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseStickerOrNone decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseNamePosition decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCompanion decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseFromOutsideTheGame decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRoom decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseHalf decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLegend decider _ _ -> Just (Decider.unwrap decider)
  Prompt.DeclareAttackers decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAttackTarget decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.DeclareBands decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExert decider _ _ -> Just (Decider.unwrap decider)
  Prompt.DeclareBlockers decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.DividePiles decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AssignCombatDamage decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTargets decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnounceTargets decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseWordSwap decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSubtype decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseParity decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseWord decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSearchZones decider _ _ -> Just (Decider.unwrap decider)
  Prompt.Search decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.CastWhileSearching decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseX decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMutateSide decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseForage decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLearn decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTimeTravel decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseClash decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEntwine decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseBuyback decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSplice decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAssistant decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAssistAmount decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseKicker decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ReturnCommander decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCommandZoneOfferFirst decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLibraryEnd decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ArrangeLibraryArrivals decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ArrangeLibraryCards decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ArrangeGraveyardArrivals decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseModes decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCopyTarget decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEntryOption decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRiot decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseUnleash decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTribute decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDredge decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLandOrNonland decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRedirect decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCombatDamageOption decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSubstitute decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePayLifeOnEntry decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRevealOnEntry decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEnlist decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseEncode decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRerolledDie decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseColor decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseDesignation decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseManaType decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardType decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCardName decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseGuess decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseOpponent decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseProtector decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePlayer decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseActivePlayer decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderTriggers decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderDamage decider _ _ -> Just (Decider.unwrap decider)
  Prompt.AllocateDamage decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReplacement decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseSacrifices decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExilesFromGraveyard decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseMaterials decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExilesTotalling decider _ _ _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePermanent decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTapsForTotalPower decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTaps decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReturns decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseExiles decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseAttachment decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTurnUpAttachment decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseCost decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePlayPermission decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseHeldPermission decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderCostComponents decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderCombatTolls decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderComponentCards decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseStickerKeeper decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseTokenKind decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderForEach decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseLoopMembers decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChoosePile decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseRepeat decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderTimestamps decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OrderManaActivations decider _ _ -> Just (Decider.unwrap decider)
  Prompt.KeepHand decider _ _ -> Just (Decider.unwrap decider)
  Prompt.DeclareMulligan decider _ _ -> Just (Decider.unwrap decider)
  Prompt.Bottom decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.MulliganAction decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OpeningHandAction decider _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseOptional decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseClause decider _ _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OfferedCast decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseOfferedCastSpell decider _ _ -> Just (Decider.unwrap decider)
  Prompt.OfferedMiracleReveal decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.OfferedExtra decider _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseToPay decider _ _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnouncePhyrexianPayment decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnounceHybridPayment decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.AnnounceHybridHalf decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseHybridHalf decider _ _ _ _ -> Just (Decider.unwrap decider)
  Prompt.ChooseReducedCost decider _ _ _ -> Just (Decider.unwrap decider)
