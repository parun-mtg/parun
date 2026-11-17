module Parun.Codec.DamageEventSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.DamageEvent as DamageEvent
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DamageEvent" $ do
  -- A NONZERO toxic value, so the CR 702.164b rider round-trips rather than
  -- getting defaulted past, and CR 702.80b's wither bit set so it round-trips as
  -- a present key too. No lifelink, no infect, so dealtByInfect and
  -- dealtByLifelink are both omitted keys. CR 702.76a's and CR 702.173a's three
  -- deal-time reads, CR 105.2's colors and CR 201.2a's names are all set away
  -- from their defaults for the same reason.
  Spec.it s "MkDamageEvent, dealt to a player" $
    Common.assertCodec
      s
      DamageEvent.codec
      DamageEvent.MkDamageEvent
        { DamageEvent.source = ObjectId.MkObjectId 1,
          DamageEvent.target = Recipient.ToPlayer (PlayerId.MkPlayerId 2),
          DamageEvent.amount = 3,
          DamageEvent.excess = 0,
          DamageEvent.dealtByDeathtouch = True,
          DamageEvent.dealtByInfect = False,
          DamageEvent.dealtByWither = True,
          DamageEvent.dealtByToxic = 2,
          DamageEvent.dealtByLifelink = Nothing,
          DamageEvent.dealtByController = Just (PlayerId.MkPlayerId 4),
          DamageEvent.dealtByCreatureTypes = Set.singleton Subtype.Rogue,
          DamageEvent.dealtByCommander = True,
          DamageEvent.dealtByCardTypes = Set.singleton CardType.Artifact,
          DamageEvent.dealtBySupertypes = Set.singleton Supertype.Legendary,
          DamageEvent.dealtByColors = Set.singleton Color.Red,
          DamageEvent.dealtByNames = Set.singleton (CardName.MkCardName (Text.pack "Blazing Effigy")),
          DamageEvent.dealtFromZone = Nothing,
          DamageEvent.dealtByTargets = Map.empty,
          DamageEvent.recipientCardTypes = Set.empty,
          DamageEvent.recipientController = Nothing,
          DamageEvent.kind = DamageKind.Combat
        }
      " {\"source\":1,\"target\":{\"type\":\"ToPlayer\",\"value\":2},\"amount\":3,\"dealtByDeathtouch\":true,\"dealtByWither\":true,\"dealtByToxic\":2,\"dealtByController\":4,\"dealtByCreatureTypes\":[\"Rogue\"],\"dealtByCommander\":true,\"dealtByCardTypes\":[\"Artifact\"],\"dealtBySupertypes\":[\"Legendary\"],\"dealtByColors\":[\"Red\"],\"dealtByNames\":[\"Blazing Effigy\"],\"kind\":\"Combat\"} "
  -- CR 120.3c's recipient tag and CR 608's noncombat damage are each the other
  -- arm of their type. CR 702.15b's lifelink payee is a concrete PlayerId, and
  -- CR 120.10's excess, CR 400.1's zone, CR 115.1's targets and CR 120.3's recipient card types
  -- and controller are away from their defaults.
  Spec.it s "MkDamageEvent, dealt to a planeswalker, with lifelink" $
    Common.assertCodec
      s
      DamageEvent.codec
      DamageEvent.MkDamageEvent
        { DamageEvent.source = ObjectId.MkObjectId 1,
          DamageEvent.target = Recipient.ToPlaneswalker (ObjectId.MkObjectId 5),
          DamageEvent.amount = 4,
          DamageEvent.excess = 1,
          DamageEvent.dealtByDeathtouch = False,
          DamageEvent.dealtByInfect = True,
          DamageEvent.dealtByWither = False,
          DamageEvent.dealtByToxic = 0,
          DamageEvent.dealtByLifelink = Just (PlayerId.MkPlayerId 2),
          DamageEvent.dealtByController = Nothing,
          DamageEvent.dealtByCreatureTypes = Set.empty,
          DamageEvent.dealtByCommander = False,
          DamageEvent.dealtByCardTypes = Set.empty,
          DamageEvent.dealtBySupertypes = Set.empty,
          DamageEvent.dealtByColors = Set.empty,
          DamageEvent.dealtByNames = Set.empty,
          DamageEvent.dealtFromZone = Just Zone.Stack,
          DamageEvent.dealtByTargets = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.singleton (Recipient.ToPlaneswalker (ObjectId.MkObjectId 5))),
          DamageEvent.recipientCardTypes = Set.singleton CardType.Planeswalker,
          DamageEvent.recipientController = Just (PlayerId.MkPlayerId 3),
          DamageEvent.kind = DamageKind.Noncombat
        }
      " {\"source\":1,\"target\":{\"type\":\"ToPlaneswalker\",\"value\":5},\"amount\":4,\"excess\":1,\"dealtByInfect\":true,\"dealtByLifelink\":2,\"dealtFromZone\":\"Stack\",\"dealtByTargets\":{\"target\":[{\"type\":\"ToPlaneswalker\",\"value\":5}]},\"recipientCardTypes\":[\"Planeswalker\"],\"recipientController\":3,\"kind\":\"Noncombat\"} "
  -- Every dealtBy* rider at its default, which is what an event carrying none
  -- of them means.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertCodec
      s
      DamageEvent.codec
      DamageEvent.MkDamageEvent
        { DamageEvent.source = ObjectId.MkObjectId 1,
          DamageEvent.target = Recipient.ToPlayer (PlayerId.MkPlayerId 2),
          DamageEvent.amount = 3,
          DamageEvent.excess = 0,
          DamageEvent.dealtByDeathtouch = False,
          DamageEvent.dealtByInfect = False,
          DamageEvent.dealtByWither = False,
          DamageEvent.dealtByToxic = 0,
          DamageEvent.dealtByLifelink = Nothing,
          DamageEvent.dealtByController = Nothing,
          DamageEvent.dealtByCreatureTypes = Set.empty,
          DamageEvent.dealtByCommander = False,
          DamageEvent.dealtByCardTypes = Set.empty,
          DamageEvent.dealtBySupertypes = Set.empty,
          DamageEvent.dealtByColors = Set.empty,
          DamageEvent.dealtByNames = Set.empty,
          DamageEvent.dealtFromZone = Nothing,
          DamageEvent.dealtByTargets = Map.empty,
          DamageEvent.recipientCardTypes = Set.empty,
          DamageEvent.recipientController = Nothing,
          DamageEvent.kind = DamageKind.Combat
        }
      " {\"source\":1,\"target\":{\"type\":\"ToPlayer\",\"value\":2},\"amount\":3,\"kind\":\"Combat\"} "
