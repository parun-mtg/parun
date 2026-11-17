-- The CostAmount a cost component carries, and CR 601.2b's substitution of an
-- announced value into it: below Parun.Engine.Cost so that Parun.Engine.Mana,
-- which that module imports, can price a route at an announced count too.
module Parun.Engine.CostAmount where

import qualified Data.Functor.Const as Const
import qualified Data.Functor.Identity as Identity
import qualified Data.Monoid as Monoid
import Numeric.Natural (Natural)
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Children as Children
import qualified Parun.Types.CardsFromHand as CardsFromHand
import Parun.Types.Cost (Cost)
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CountersFromPermanents as CountersFromPermanents
import qualified Parun.Types.CountersOnThis as CountersOnThis
import qualified Parun.Types.DiscardCards as DiscardCards
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.MoveCardsFromGraveyard as MoveCardsFromGraveyard
import qualified Parun.Types.ObjectCount as ObjectCount
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.Sacrifice as Sacrifice
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TapPermanents as TapPermanents

-- CR 601.2b: substitute the value announced for one variable into every
-- component carrying it. X's mana half is Parun.Engine.Cost.substituteX's; a
-- count announced beside it (March of Wretched Sorrow's cards exiled) has no
-- other half.
substituteAnnounced :: SlotName.SlotName -> Natural -> Cost keyword -> Cost keyword
substituteAnnounced slot n cost = cost {Cost.components = fmap (fixCriteria . Identity.runIdentity . traverseAmount (Identity.Identity . fixAt)) (Cost.components cost)}
  where
    fixAt amount = if amount == CostAmount.Announced slot then CostAmount.Fixed n else amount
    fixCriteria = Identity.runIdentity . traverseCriterionReads slot (const (Identity.Identity (Operand.Literal (toInteger n))))

-- CR 107.3a / 601.2b: every comparison a component's criteria make against the
-- value announced under this slot (Sidisi, Regent of the Mire's "a creature you
-- control with mana value X"), as a traversal of the operand. Its own walk
-- rather than one of `traverseAmount`'s amounts: the variable stands in a
-- Filter there, as Operand.AmountInSlot, not as a CostAmount.
traverseCriterionReads :: (Applicative f) => SlotName.SlotName -> (Measures.Measures -> f Operand.Operand) -> CostComponent.CostComponent keyword -> f (CostComponent.CostComponent keyword)
traverseCriterionReads slot f = Children.costComponentChildren Children.costLeaves {Children.costFilter = filterReads}
  where
    filterReads predicate = case predicate of
      Filter.Measures measures | Measures.operand measures == Operand.AmountInSlot slot -> (\operand -> Filter.Measures measures {Measures.operand = operand}) <$> f measures
      _ -> Children.filterChildren (Children.descending filterReads) predicate

-- The comparisons `traverseCriterionReads` finds against this slot's value.
criterionReads :: SlotName.SlotName -> CostComponent.CostComponent keyword -> [Measures.Measures]
criterionReads slot = Const.getConst . traverseCriterionReads slot (Const.Const . pure)

-- `criterionReads` of X.
criterionReadsOfX :: CostComponent.CostComponent keyword -> [Measures.Measures]
criterionReadsOfX = criterionReads Binding.variableX

-- The CostAmount a component carries, as a traversal: the one place that knows
-- which components' amounts CR 107.3a's X can stand in, so Parun.Engine.Cost's
-- substituteX and componentHasVariable cannot disagree about it. A criterion's
-- X is `traverseCriterionReads`'.
--
-- EXHAUSTIVE with no wildcard, this module's posture for every CostComponent
-- match: a new component owes an answer here, and -Werror is what makes it.
traverseAmount :: (Applicative f) => (CostAmount.CostAmount -> f CostAmount.CostAmount) -> CostComponent.CostComponent keyword -> f (CostComponent.CostComponent keyword)
traverseAmount f component = case component of
  CostComponent.PayLife amount -> CostComponent.PayLife <$> f amount
  CostComponent.PayEnergy amount -> CostComponent.PayEnergy <$> f amount
  CostComponent.RemoveLoyaltyFromThis amount -> CostComponent.RemoveLoyaltyFromThis <$> f amount
  CostComponent.Blight amount -> CostComponent.Blight <$> f amount
  -- The announcement fixes CR 701.67b's ceiling exactly as it fixes the {X}
  -- that licence scopes.
  CostComponent.Waterbend amount -> CostComponent.Waterbend <$> f amount
  CostComponent.Sacrifice sacrifice -> (\count -> CostComponent.Sacrifice sacrifice {Sacrifice.count = count}) <$> traverseCount f (Sacrifice.count sacrifice)
  CostComponent.RemoveCounters remove -> (\amount -> CostComponent.RemoveCounters remove {CountersFromPermanents.count = amount}) <$> traverseCount f (CountersFromPermanents.count remove)
  CostComponent.PayComputedLife _ -> pure component
  CostComponent.TapThis -> pure component
  CostComponent.UntapThis -> pure component
  CostComponent.ExertThis -> pure component
  CostComponent.SacrificeThis -> pure component
  CostComponent.ReturnThis -> pure component
  CostComponent.TapForTotalPower {} -> pure component
  CostComponent.TapPermanents tap -> (\amount -> CostComponent.TapPermanents tap {TapPermanents.count = amount}) <$> f (TapPermanents.count tap)
  CostComponent.ReturnPermanents {} -> pure component
  CostComponent.ExilePermanents {} -> pure component
  CostComponent.DiscardCards discard -> (\count -> CostComponent.DiscardCards discard {DiscardCards.count = count}) <$> traverseCount f (DiscardCards.count discard)
  CostComponent.DiscardThis _ -> pure component
  CostComponent.PutCardFromHandOntoBattlefield _ -> pure component
  CostComponent.AddLoyaltyToThis _ -> pure component
  CostComponent.RemoveCountersFromThis counters -> (\amount -> CostComponent.RemoveCountersFromThis counters {CountersOnThis.count = amount}) <$> f (CountersOnThis.count counters)
  -- Krumar Initiate's "endures X" (CR 107.3a, 701.63a).
  CostComponent.PutCountersOnThis counters -> (\amount -> CostComponent.PutCountersOnThis counters {CountersOnThis.count = amount}) <$> f (CountersOnThis.count counters)
  CostComponent.PutCountersOnPayer _ -> pure component
  -- Nullary: CR 701.61a states no number at all, so there is nothing for CR
  -- 601.2b to announce.
  CostComponent.Forage -> pure component
  CostComponent.FlipCoin -> pure component
  -- CR 702.174a's cost names no X.
  CostComponent.ChooseOpponent _ -> pure component
  CostComponent.ChooseSubtype _ -> pure component
  CostComponent.WaterbendInstead _ -> pure component
  CostComponent.TapInsteadOfMana _ -> pure component
  CostComponent.VariableAtLeast _ -> pure component
  CostComponent.PayManaCostOf _ -> pure component
  CostComponent.ExileThisFrom _ -> pure component
  CostComponent.ExileThis -> pure component
  CostComponent.MoveCardsFromGraveyard exile -> (\amount -> CostComponent.MoveCardsFromGraveyard exile {MoveCardsFromGraveyard.count = amount}) <$> f (MoveCardsFromGraveyard.count exile)
  CostComponent.ExileMaterials {} -> pure component
  CostComponent.ExileTopFromGraveyard _ -> pure component
  CostComponent.PutExiledIntoGraveyard _ -> pure component
  -- Incinerator of the Guilty's "collect evidence X" (CR 107.3f, 701.59a).
  CostComponent.CollectEvidence amount -> CostComponent.CollectEvidence <$> f amount
  CostComponent.ExileCardsTotalling _ -> pure component
  -- CR 601.2f's computed amount, not CR 601.2b's X: fixComputed reads it once the
  -- targets exist.
  CostComponent.CollectEvidenceOfTargets -> pure component
  CostComponent.ExileCardsFromHand exile -> (\amount -> CostComponent.ExileCardsFromHand exile {CardsFromHand.count = amount}) <$> f (CardsFromHand.count exile)
  CostComponent.RevealCardsFromHand reveal -> (\amount -> CostComponent.RevealCardsFromHand reveal {CardsFromHand.count = amount}) <$> f (CardsFromHand.count reveal)
  CostComponent.Behold _ -> pure component
  CostComponent.BeholdAndExile _ -> pure component
  CostComponent.MillCards _ -> pure component
  CostComponent.RevealTopOfLibrary _ -> pure component
  CostComponent.RevealSecretChoice -> pure component

-- `traverseAmount` into an ObjectCount: Every names no amount to announce.
traverseCount :: (Applicative f) => (CostAmount.CostAmount -> f CostAmount.CostAmount) -> ObjectCount.ObjectCount -> f ObjectCount.ObjectCount
traverseCount f count = case count of
  ObjectCount.Exactly amount -> ObjectCount.Exactly <$> f amount
  ObjectCount.Every -> pure ObjectCount.Every

-- Parun.Engine.Cost.substituteX's predicate half, read off the same two
-- traversals: a component this answers False for is one no announcement will
-- ever substitute.
componentHasVariable :: CostComponent.CostComponent keyword -> Bool
componentHasVariable component = Monoid.getAny (Const.getConst (traverseAmount (Const.Const . Monoid.Any . (== CostAmount.Announced Binding.variableX)) component)) || not (null (criterionReadsOfX component))
