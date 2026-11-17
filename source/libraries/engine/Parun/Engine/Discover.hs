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
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Parun.Types.Zone as Zone

-- | CR 701.57a for this N, in written order, is walk then afterWalk: exile
-- until a nonland card with mana value N or less; offer its cast without paying
-- its mana cost, the resulting spell's mana value N or less; put it into the
-- hand if it is still in exile; put the rest on the bottom in a random order.
-- Two pieces so the arm can bind CR 701.57c's discovered card between them,
-- while every card the walk exiled is still where it arrived.
--
-- The OFFER and the HAND move read only a card the walk STOPPED on, so a walk
-- that ran the library out (CR 701.57b's "impossible") offers nothing and puts
-- nothing into the hand. The data\/scenarios\/investigate discover scenarios
-- prove the offer, the hand and the bottom.
--
-- The walk binds every card it exiled at Binding.discoveredCards. The player
-- is the discovering one, whose library the walk reads and who is offered the
-- cast (CR 701.57a); the Zoyowa's Justice scenarios prove both.
walk :: PlayerRef.PlayerRef -> Integer -> Effect.Effect Card (GrantedAbility.GrantedAbility Card)
walk discoverer n =
  Effect.MoveToZone (move (ObjectRef.TopOfLibraryUntil exiling) Zone.Exile LibraryPlacement.defaultValue (Just Binding.discoveredCards))
  where
    exiling = TopOfLibraryUntil.MkTopOfLibraryUntil {TopOfLibraryUntil.player = discoverer, TopOfLibraryUntil.filter = stopsOn n, TopOfLibraryUntil.weight = Quantity.Literal 1, TopOfLibraryUntil.count = Quantity.Literal 1, TopOfLibraryUntil.deck = Nothing}

-- | CR 701.57a / 701.57c: what the walk stops on, a nonland card with mana
-- value N or less. Among the cards it exiled, only the discovered card passes.
stopsOn :: Integer -> Filter.Filter Keyword.Keyword
stopsOn n = Filter.And [Filter.Not (Filter.HasCardType CardType.Land), atMost n]

-- | The process after the walk: the offer, the hand move and the bottom.
afterWalk :: PlayerRef.PlayerRef -> Integer -> Seq.Seq (Effect.Effect Card (GrantedAbility.GrantedAbility Card))
afterWalk discoverer n =
  Seq.fromList
    [ Effect.OfferCast
        OfferCast.MkOfferCast
          { OfferCast.ref = discovered,
            OfferCast.caster = discoverer,
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
    discovered = ObjectRef.EachCardFromAmong EachCardFromAmong.MkEachCardFromAmong {EachCardFromAmong.slot = Binding.discoveredCards, EachCardFromAmong.filter = stopsOn n}
    free =
      CastOffer.MkCastOffer
        { CastOffer.transformed = False,
          CastOffer.withoutPayingManaCost = True,
          CastOffer.payingInstead = Nothing,
          CastOffer.spending = ManaSpending.AsProduced,
          CastOffer.restriction = Just (atMost n),
          CastOffer.offeredBy = Nothing,
          CastOffer.faceDown = Nothing,
          CastOffer.payableFromSpent = False,
          CastOffer.reduction = 0
        }

atMost :: Integer -> Filter.Filter Keyword.Keyword
atMost n = Filter.Measures Measures.MkMeasures {Measures.measure = Measure.ManaValue, Measures.comparison = Comparison.AtMost, Measures.operand = Operand.Literal n}

move :: ObjectRef.ObjectRef -> Zone.Zone -> LibraryPlacement.LibraryPlacement -> Maybe SlotName.SlotName -> MoveToZone.MoveToZone (GrantedAbility.GrantedAbility Card)
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
