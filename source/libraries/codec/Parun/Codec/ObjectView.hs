{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ObjectView where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Parun.Codec.AbilityView as AbilityView
import qualified Parun.Codec.Acquisition as Acquisition
import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.CardType as CardType
import qualified Parun.Codec.ClassLevel as ClassLevel
import qualified Parun.Codec.Color as Color
import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.Designation as Designation
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.ManaCost as ManaCost
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerChoice as PlayerChoice
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Codec.Recipient as Recipient
import qualified Parun.Codec.RoomHalf as RoomHalf
import qualified Parun.Codec.RoomIndex as RoomIndex
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.Codec.TapState as TapState
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ObjectView as ObjectView

-- | A bare object keyed by the record's field names; `characteristics` is
-- null for an object the seat may not see, and `face` for any but a face-down
-- one the seat may look at; `foretellCostReduction` is null for anything but a
-- card given a foretell cost the seat may look at; `ability` is null for
-- anything but an ability on the stack; `targets` is empty for anything but a
-- spell or ability on the stack with a target; each chosen field is omitted
-- when empty, Parun.Codec.Object's keys and shapes otherwise.
codec :: Codec.Codec ObjectView.ObjectView
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec ObjectView.object
  owner <- Fields.required "owner" PlayerId.codec ObjectView.owner
  controller <- Fields.required "controller" (Common.maybe PlayerId.codec) ObjectView.controller
  tapped <- Fields.required "tapped" TapState.flag ObjectView.tapped
  damage <- Fields.required "damage" Common.natural ObjectView.damage
  counters <- Fields.required "counters" (Common.multiset (CounterKind.codec Keyword.codec)) ObjectView.counters
  attachedTo <- Fields.required "attachedTo" (Common.maybe Recipient.codec) ObjectView.attachedTo
  characteristics <- Fields.required "characteristics" (Common.maybe ProjectedCharacteristics.codec) ObjectView.characteristics
  face <- Fields.required "face" (Common.maybe ProjectedCharacteristics.codec) ObjectView.face
  foretellCostReduction <- Fields.required "foretellCostReduction" (Common.maybe ManaCost.codec) ObjectView.foretellCostReduction
  ability <- Fields.required "ability" (Common.maybe AbilityView.codec) ObjectView.ability
  targets <- Fields.required "targets" (Common.list Recipient.codec) ObjectView.targets
  summoningSick <- Fields.defaulted "summoningSick" False Common.boolean ObjectView.summoningSick
  ringBearerFor <- Fields.defaulted "ringBearerFor" Nothing (Common.maybe PlayerId.codec) ObjectView.ringBearerFor
  protector <- Fields.defaulted "protector" Nothing (Common.maybe PlayerId.codec) ObjectView.protector
  classLevel <- Fields.defaulted "classLevel" Nothing (Common.maybe ClassLevel.codec) ObjectView.classLevel
  unlockedHalves <- Fields.defaulted "unlockedHalves" Set.empty (Common.set RoomHalf.codec) ObjectView.unlockedHalves
  designations <- Fields.defaulted "designations" Set.empty (Common.set Designation.codec) ObjectView.designations
  ventureRoom <- Fields.defaulted "ventureRoom" Nothing (Common.maybe RoomIndex.codec) ObjectView.ventureRoom
  chosenColors <- Fields.defaulted "chosenColors" Map.empty (Common.keyedList (Common.keyValue Acquisition.codec (Common.set Color.codec))) ObjectView.chosenColors
  chosenColorless <- Fields.defaulted "chosenColorless" Map.empty (Common.keySet Acquisition.codec) ObjectView.chosenColorless
  chosenSubtype <- Fields.defaulted "chosenSubtype" Map.empty (Common.keyedList (Common.keyValue Acquisition.codec (Common.nonEmpty Subtype.codec))) ObjectView.chosenSubtype
  chosenCardType <- Fields.defaulted "chosenCardType" Map.empty (Common.keyedList (Common.keyValue Acquisition.codec CardType.codec)) ObjectView.chosenCardType
  chosenNames <- Fields.defaulted "chosenNames" Map.empty (Common.keyedList (Common.keyValue Acquisition.codec (Common.naturalMap PlayerId.codec (Common.set CardName.codec)))) ObjectView.chosenNames
  chosenPlayers <- Fields.defaulted "chosenPlayers" Map.empty (Common.keyedList (Common.keyValue Acquisition.codec (Common.keyedList (Common.keyValue PlayerChoice.codec PlayerId.codec)))) ObjectView.chosenPlayers
  chosenNumber <- Fields.defaulted "chosenNumber" Map.empty (Common.keyedList (Common.keyValue Acquisition.codec Common.natural)) ObjectView.chosenNumber
  chosenObjects <- Fields.defaulted "chosenObjects" Map.empty (Common.keyedList (Common.keyValue Acquisition.codec (Common.set ObjectId.codec))) ObjectView.chosenObjects
  pure
    ObjectView.MkObjectView
      { ObjectView.object = object,
        ObjectView.owner = owner,
        ObjectView.controller = controller,
        ObjectView.tapped = tapped,
        ObjectView.damage = damage,
        ObjectView.counters = counters,
        ObjectView.attachedTo = attachedTo,
        ObjectView.characteristics = characteristics,
        ObjectView.face = face,
        ObjectView.foretellCostReduction = foretellCostReduction,
        ObjectView.ability = ability,
        ObjectView.targets = targets,
        ObjectView.summoningSick = summoningSick,
        ObjectView.ringBearerFor = ringBearerFor,
        ObjectView.protector = protector,
        ObjectView.classLevel = classLevel,
        ObjectView.unlockedHalves = unlockedHalves,
        ObjectView.designations = designations,
        ObjectView.ventureRoom = ventureRoom,
        ObjectView.chosenColors = chosenColors,
        ObjectView.chosenColorless = chosenColorless,
        ObjectView.chosenSubtype = chosenSubtype,
        ObjectView.chosenCardType = chosenCardType,
        ObjectView.chosenNames = chosenNames,
        ObjectView.chosenPlayers = chosenPlayers,
        ObjectView.chosenNumber = chosenNumber,
        ObjectView.chosenObjects = chosenObjects
      }
