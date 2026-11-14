-- | CR 701.57, discover: the whole of the keyword action, written once.
--
-- Rule 701 is a keyword-action rule exactly as rule 702 is a keyword rule, so
-- the process lives here rather than spelled out on each card, and CR 701.57b's
-- "has discovered" is one event however a card asks for it. The process is
-- built from the effect DSL's own instructions, Parun.Engine.Keyword's
-- warpExile posture: rule 701.57a is four of them in written order, and the
-- arm that runs them (Parun.Engine.Resolve.Arms.Zones.discover) never learns
-- which.
module Parun.Engine.Discover where

import qualified Data.Sequence as Seq
import qualified Parun.Engine.Binding as Binding
import Parun.Types.Card (Card)
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CastObligation as CastObligation
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.CastRepetition as CastRepetition
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.EachCardFromAmong as EachCardFromAmong
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LibraryPlacement as LibraryPlacement
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.MoveToZone as MoveToZone
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.OfferCast as OfferCast
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.PermissionVerb as PermissionVerb
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.Types.Zone as Zone

-- | CR 701.57a for this N, in written order: exile until a nonland card with
-- mana value N or less; offer its cast without paying its mana cost, the
-- resulting spell's mana value N or less; put it into the hand if it is still
-- in exile; put the rest on the bottom in a random order.
--
-- The OFFER and the HAND move read only a card the walk STOPPED on, so a walk
-- that ran the library out (CR 701.57b's "impossible") offers nothing and puts
-- nothing into the hand. The data\/scenarios\/investigate discover scenarios
-- prove the offer, the hand and the bottom.
process :: Integer -> Seq.Seq (Effect.Effect Card (GrantedAbility.GrantedAbility Card))
process n =
  Seq.fromList
    [ Effect.MoveToZone (move (ObjectRef.TopOfLibraryUntil walk) Zone.Exile LibraryPlacement.defaultValue (Just Binding.discoveredCards)),
      Effect.OfferCast
        OfferCast.MkOfferCast
          { OfferCast.ref = discovered,
            OfferCast.caster = you,
            OfferCast.optionality = CastObligation.Optional,
            OfferCast.verb = PermissionVerb.Cast,
            OfferCast.offer = free,
            OfferCast.repetition = CastRepetition.Once,
            OfferCast.copied = False,
            OfferCast.controlWhileResolving = False,
            OfferCast.slot = Nothing
          },
      Effect.MoveToZone (move discovered Zone.Hand LibraryPlacement.defaultValue Nothing),
      Effect.MoveToZone (move (ObjectRef.InSlot Binding.discoveredCards) Zone.Library (LibraryPlacement.RandomOrder LibraryPosition.Bottom) Nothing)
    ]
  where
    you = PlayerRef.Relative PlayerRelation.You
    atMostN = Filter.Measures Measures.MkMeasures {Measures.measure = Measure.ManaValue, Measures.comparison = Comparison.AtMost, Measures.operand = Operand.Literal n}
    stopsOn :: Filter.Filter Keyword.Keyword
    stopsOn = Filter.And [Filter.Not (Filter.HasCardType CardType.Land), atMostN]
    walk = TopOfLibraryUntil.MkTopOfLibraryUntil {TopOfLibraryUntil.player = you, TopOfLibraryUntil.filter = stopsOn, TopOfLibraryUntil.weight = Quantity.Literal 1, TopOfLibraryUntil.count = Quantity.Literal 1, TopOfLibraryUntil.deck = Nothing}
    discovered = ObjectRef.EachCardFromAmong EachCardFromAmong.MkEachCardFromAmong {EachCardFromAmong.slot = Binding.discoveredCards, EachCardFromAmong.filter = stopsOn}
    free =
      CastOffer.MkCastOffer
        { CastOffer.transformed = False,
          CastOffer.withoutPayingManaCost = True,
          CastOffer.payingInstead = Nothing,
          CastOffer.spending = ManaSpending.AsProduced,
          CastOffer.restriction = Just atMostN,
          CastOffer.offeredBy = Nothing,
          CastOffer.faceDown = Nothing,
          CastOffer.payableFromSpent = False,
          CastOffer.reduction = 0
        }
    move ref zone placement bound =
      MoveToZone.MkMoveToZone
        { MoveToZone.ref = ref,
          MoveToZone.zone = zone,
          MoveToZone.riders = EntryRiders.defaultValue,
          MoveToZone.slot = bound,
          MoveToZone.origin = Nothing,
          MoveToZone.placement = placement,
          MoveToZone.duration = Nothing
        }
