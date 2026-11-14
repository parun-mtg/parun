module Parun.Codec.CostComponent where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Behold as Behold
import qualified Parun.Codec.CardsFromHand as CardsFromHand
import qualified Parun.Codec.ComputedLife as ComputedLife
import qualified Parun.Codec.CostAmount as CostAmount
import qualified Parun.Codec.CountersFromPermanents as CountersFromPermanents
import qualified Parun.Codec.CountersOnPayer as CountersOnPayer
import qualified Parun.Codec.CountersOnThis as CountersOnThis
import qualified Parun.Codec.DiscardCards as DiscardCards
import qualified Parun.Codec.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Parun.Codec.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Codec.ExileMaterials as ExileMaterials
import qualified Parun.Codec.ExilePermanents as ExilePermanents
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Gift as Gift
import qualified Parun.Codec.ReturnPermanents as ReturnPermanents
import qualified Parun.Codec.Sacrifice as Sacrifice
import qualified Parun.Codec.SubtypeFamily as SubtypeFamily
import qualified Parun.Codec.TapForTotalPower as TapForTotalPower
import qualified Parun.Codec.TapPermanents as TapPermanents
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.DiscardCause as DiscardCause

-- | Tagged rather than bare-nullary from the start: this family grows
-- payload-carrying constructors (PayLife, Sacrifice), so it is built from
-- 'Arm.tagged' rather than delegated to a nullary-table helper -- which derives
-- the encoder, the decoder and the schema from the list below, so a new arm is
-- one entry and nothing else. 'tagOf' below names every constructor, so a
-- new one is a @-Wincomplete-patterns@ error here; what it does not force is the
-- matching ARM, and a constructor whose tag has none encodes as @{}@.
--
-- The keyword codec is a PARAMETER; see Parun.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CostComponent.CostComponent keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.nullary "TapThis" CostComponent.TapThis,
      Arm.nullary "UntapThis" CostComponent.UntapThis,
      Arm.nullary "SacrificeThis" CostComponent.SacrificeThis,
      Arm.nullary "ReturnThis" CostComponent.ReturnThis,
      Arm.payload "PayLife" CostAmount.codec CostComponent.PayLife (\x -> case x of CostComponent.PayLife y -> Just y; _ -> Nothing),
      Arm.payload "PayComputedLife" ComputedLife.codec CostComponent.PayComputedLife (\x -> case x of CostComponent.PayComputedLife y -> Just y; _ -> Nothing),
      Arm.payload "Sacrifice" (Sacrifice.codec keywordCodec) CostComponent.Sacrifice (\x -> case x of CostComponent.Sacrifice y -> Just y; _ -> Nothing),
      Arm.payload "TapForTotalPower" (TapForTotalPower.codec keywordCodec) CostComponent.TapForTotalPower (\x -> case x of CostComponent.TapForTotalPower y -> Just y; _ -> Nothing),
      Arm.payload "TapPermanents" (TapPermanents.codec keywordCodec) CostComponent.TapPermanents (\x -> case x of CostComponent.TapPermanents y -> Just y; _ -> Nothing),
      Arm.payload "ReturnPermanents" (ReturnPermanents.codec keywordCodec) CostComponent.ReturnPermanents (\x -> case x of CostComponent.ReturnPermanents y -> Just y; _ -> Nothing),
      Arm.payload "ExilePermanents" (ExilePermanents.codec keywordCodec) CostComponent.ExilePermanents (\x -> case x of CostComponent.ExilePermanents y -> Just y; _ -> Nothing),
      Arm.payload "DiscardCards" (DiscardCards.codec keywordCodec) CostComponent.DiscardCards (\x -> case x of CostComponent.DiscardCards y -> Just y; _ -> Nothing),
      -- The component carries a DiscardCause, but the WIRE does not: a card
      -- prints "Discard this card" and never says the discard is a cycle, which
      -- CR 702.29c makes true only of a cycling ability's cost -- and a cycling
      -- ability is minted from the keyword by Parun.Engine.Keyword rather than
      -- authored. So this stays nullary over the Ordinary cause, which leaves
      -- ToPayCyclingCost unspellable by card data. Faerie Macabre is the card
      -- that writes this arm, and Parun.ActivateSpec's "CR 702.29c an authored
      -- discard-this cost is not a cycle" is what proves the cause it decodes to
      -- -- a round trip cannot, the wire having one spelling for both causes.
      Arm.nullary "DiscardThis" (CostComponent.DiscardThis DiscardCause.Ordinary),
      Arm.payload "PutCardFromHandOntoBattlefield" (Filter.codec keywordCodec) CostComponent.PutCardFromHandOntoBattlefield (\x -> case x of CostComponent.PutCardFromHandOntoBattlefield y -> Just y; _ -> Nothing),
      Arm.payload "PayEnergy" CostAmount.codec CostComponent.PayEnergy (\x -> case x of CostComponent.PayEnergy y -> Just y; _ -> Nothing),
      Arm.payload "AddLoyaltyToThis" Common.natural CostComponent.AddLoyaltyToThis (\x -> case x of CostComponent.AddLoyaltyToThis y -> Just y; _ -> Nothing),
      Arm.payload "RemoveLoyaltyFromThis" CostAmount.codec CostComponent.RemoveLoyaltyFromThis (\x -> case x of CostComponent.RemoveLoyaltyFromThis y -> Just y; _ -> Nothing),
      Arm.payload "RemoveCountersFromThis" (CountersOnThis.codec keywordCodec) CostComponent.RemoveCountersFromThis (\x -> case x of CostComponent.RemoveCountersFromThis y -> Just y; _ -> Nothing),
      Arm.payload "RemoveCounters" (CountersFromPermanents.codec keywordCodec) CostComponent.RemoveCounters (\x -> case x of CostComponent.RemoveCounters y -> Just y; _ -> Nothing),
      Arm.payload "PutCountersOnThis" (CountersOnThis.codec keywordCodec) CostComponent.PutCountersOnThis (\x -> case x of CostComponent.PutCountersOnThis y -> Just y; _ -> Nothing),
      Arm.payload "PutCountersOnPayer" CountersOnPayer.codec CostComponent.PutCountersOnPayer (\x -> case x of CostComponent.PutCountersOnPayer y -> Just y; _ -> Nothing),
      Arm.payload "Blight" CostAmount.codec CostComponent.Blight (\x -> case x of CostComponent.Blight y -> Just y; _ -> Nothing),
      Arm.nullary "Forage" CostComponent.Forage,
      Arm.nullary "FlipCoin" CostComponent.FlipCoin,
      Arm.payload "ExileThisFrom" Zone.codec CostComponent.ExileThisFrom (\x -> case x of CostComponent.ExileThisFrom y -> Just y; _ -> Nothing),
      Arm.nullary "ExileThis" CostComponent.ExileThis,
      Arm.payload "ExileCardsFromGraveyard" (ExileCardsFromGraveyard.codec keywordCodec) CostComponent.ExileCardsFromGraveyard (\x -> case x of CostComponent.ExileCardsFromGraveyard y -> Just y; _ -> Nothing),
      Arm.payload "ExileMaterials" (ExileMaterials.codec keywordCodec) CostComponent.ExileMaterials (\x -> case x of CostComponent.ExileMaterials y -> Just y; _ -> Nothing),
      Arm.payload "CollectEvidence" Common.natural CostComponent.CollectEvidence (\x -> case x of CostComponent.CollectEvidence y -> Just y; _ -> Nothing),
      Arm.nullary "CollectEvidenceOfTargets" CostComponent.CollectEvidenceOfTargets,
      Arm.payload "ExileCardsTotalling" (ExileCardsTotalling.codec keywordCodec) CostComponent.ExileCardsTotalling (\x -> case x of CostComponent.ExileCardsTotalling y -> Just y; _ -> Nothing),
      Arm.payload "ExileTopFromGraveyard" (Filter.codec keywordCodec) CostComponent.ExileTopFromGraveyard (\x -> case x of CostComponent.ExileTopFromGraveyard y -> Just y; _ -> Nothing),
      Arm.payload "PutExiledIntoGraveyard" (Filter.codec keywordCodec) CostComponent.PutExiledIntoGraveyard (\x -> case x of CostComponent.PutExiledIntoGraveyard y -> Just y; _ -> Nothing),
      Arm.payload "ExileCardsFromHand" (CardsFromHand.codec keywordCodec) CostComponent.ExileCardsFromHand (\x -> case x of CostComponent.ExileCardsFromHand y -> Just y; _ -> Nothing),
      Arm.payload "RevealCardsFromHand" (CardsFromHand.codec keywordCodec) CostComponent.RevealCardsFromHand (\x -> case x of CostComponent.RevealCardsFromHand y -> Just y; _ -> Nothing),
      Arm.payload "Behold" (Behold.codec keywordCodec) CostComponent.Behold (\x -> case x of CostComponent.Behold y -> Just y; _ -> Nothing),
      Arm.payload "BeholdAndExile" (Filter.codec keywordCodec) CostComponent.BeholdAndExile (\x -> case x of CostComponent.BeholdAndExile y -> Just y; _ -> Nothing),
      Arm.payload "MillCards" Common.natural CostComponent.MillCards (\x -> case x of CostComponent.MillCards y -> Just y; _ -> Nothing),
      Arm.payload "RevealTopOfLibrary" Common.natural CostComponent.RevealTopOfLibrary (\x -> case x of CostComponent.RevealTopOfLibrary y -> Just y; _ -> Nothing),
      Arm.payload "ChooseOpponent" Gift.codec CostComponent.ChooseOpponent (\x -> case x of CostComponent.ChooseOpponent y -> Just y; _ -> Nothing),
      Arm.payload "ChooseSubtype" SubtypeFamily.codec CostComponent.ChooseSubtype (\x -> case x of CostComponent.ChooseSubtype y -> Just y; _ -> Nothing),
      Arm.payload "Waterbend" CostAmount.codec CostComponent.Waterbend (\x -> case x of CostComponent.Waterbend y -> Just y; _ -> Nothing),
      Arm.payload "WaterbendInstead" Common.natural CostComponent.WaterbendInstead (\x -> case x of CostComponent.WaterbendInstead y -> Just y; _ -> Nothing),
      Arm.payload "TapInsteadOfMana" (Filter.codec keywordCodec) CostComponent.TapInsteadOfMana (\x -> case x of CostComponent.TapInsteadOfMana y -> Just y; _ -> Nothing),
      Arm.payload "VariableAtLeast" Common.natural CostComponent.VariableAtLeast (\x -> case x of CostComponent.VariableAtLeast y -> Just y; _ -> Nothing)
    ]

tagOf :: CostComponent.CostComponent keyword -> String
tagOf x = case x of
  CostComponent.TapThis {} -> "TapThis"
  CostComponent.UntapThis {} -> "UntapThis"
  CostComponent.SacrificeThis {} -> "SacrificeThis"
  CostComponent.ReturnThis {} -> "ReturnThis"
  CostComponent.PayLife {} -> "PayLife"
  CostComponent.PayComputedLife {} -> "PayComputedLife"
  CostComponent.Sacrifice {} -> "Sacrifice"
  CostComponent.TapForTotalPower {} -> "TapForTotalPower"
  CostComponent.TapPermanents {} -> "TapPermanents"
  CostComponent.ReturnPermanents {} -> "ReturnPermanents"
  CostComponent.ExilePermanents {} -> "ExilePermanents"
  CostComponent.DiscardCards {} -> "DiscardCards"
  CostComponent.DiscardThis {} -> "DiscardThis"
  CostComponent.PutCardFromHandOntoBattlefield {} -> "PutCardFromHandOntoBattlefield"
  CostComponent.PayEnergy {} -> "PayEnergy"
  CostComponent.AddLoyaltyToThis {} -> "AddLoyaltyToThis"
  CostComponent.RemoveLoyaltyFromThis {} -> "RemoveLoyaltyFromThis"
  CostComponent.RemoveCountersFromThis {} -> "RemoveCountersFromThis"
  CostComponent.RemoveCounters {} -> "RemoveCounters"
  CostComponent.PutCountersOnThis {} -> "PutCountersOnThis"
  CostComponent.PutCountersOnPayer {} -> "PutCountersOnPayer"
  CostComponent.Blight {} -> "Blight"
  CostComponent.Forage {} -> "Forage"
  CostComponent.FlipCoin {} -> "FlipCoin"
  CostComponent.ExileThisFrom {} -> "ExileThisFrom"
  CostComponent.ExileThis {} -> "ExileThis"
  CostComponent.ExileCardsFromGraveyard {} -> "ExileCardsFromGraveyard"
  CostComponent.ExileMaterials {} -> "ExileMaterials"
  CostComponent.CollectEvidence {} -> "CollectEvidence"
  CostComponent.CollectEvidenceOfTargets {} -> "CollectEvidenceOfTargets"
  CostComponent.ExileCardsTotalling {} -> "ExileCardsTotalling"
  CostComponent.ExileTopFromGraveyard {} -> "ExileTopFromGraveyard"
  CostComponent.PutExiledIntoGraveyard {} -> "PutExiledIntoGraveyard"
  CostComponent.ExileCardsFromHand {} -> "ExileCardsFromHand"
  CostComponent.RevealCardsFromHand {} -> "RevealCardsFromHand"
  CostComponent.Behold {} -> "Behold"
  CostComponent.BeholdAndExile {} -> "BeholdAndExile"
  CostComponent.MillCards {} -> "MillCards"
  CostComponent.RevealTopOfLibrary {} -> "RevealTopOfLibrary"
  CostComponent.ChooseOpponent {} -> "ChooseOpponent"
  CostComponent.ChooseSubtype {} -> "ChooseSubtype"
  CostComponent.Waterbend {} -> "Waterbend"
  CostComponent.WaterbendInstead {} -> "WaterbendInstead"
  CostComponent.TapInsteadOfMana {} -> "TapInsteadOfMana"
  CostComponent.VariableAtLeast {} -> "VariableAtLeast"
