module Parun.Types.ObjectView where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.AbilityView as AbilityView
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ClassLevel as ClassLevel
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Designation as Designation
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RoomHalf as RoomHalf
import qualified Parun.Types.RoomIndex as RoomIndex
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.TapState as TapState

-- | CR 400.2 / 708.2: an object as one seat sees it, its status always, its
-- characteristics only when that seat may see them, and the card behind a
-- face-down object only when that seat may look at it.
data ObjectView = MkObjectView
  { object :: ObjectId.ObjectId,
    owner :: PlayerId.PlayerId,
    controller :: Maybe PlayerId.PlayerId,
    tapped :: TapState.TapState,
    damage :: Natural.Natural,
    counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural,
    attachedTo :: Maybe Recipient.Recipient,
    characteristics :: Maybe ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 406.3 / 708.5: the card a face-down object is, for a seat that may look at it.
    face :: Maybe ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 702.143d / 702.143e: what a granted foretell cost takes off the card's mana cost, for a seat that may look at it.
    foretellCostReduction :: Maybe ManaCost.ManaCost,
    -- | CR 113.7a / 602.2a: what an ability on the stack came from, and Nothing for anything else.
    ability :: Maybe AbilityView.AbilityView,
    -- | CR 601.2c / 602.2b / 603.3d: what a spell or ability on the stack targets, and empty for anything else.
    targets :: [Recipient.Recipient],
    -- | CR 302.6: a creature on the battlefield that can't attack or pay {T} or {Q} costs yet, its controller not having controlled it since their most recent turn began and it lacking haste.
    summoningSick :: Bool,
    -- | CR 701.54b: the player the permanent is the Ring-bearer for.
    ringBearerFor :: Maybe PlayerId.PlayerId,
    -- | CR 717.2b: a Class's level, Nothing where it was never levelled.
    classLevel :: Maybe ClassLevel.ClassLevel,
    -- | CR 709.5c: a Room's unlocked halves.
    unlockedHalves :: Set.Set RoomHalf.RoomHalf,
    -- | CR 702.112b / 701.37b / 701.60b / 720.3b and the rest of
    -- Parun.Types.Designation: the permanent's designations.
    designations :: Set.Set Designation.Designation,
    -- | CR 309.4: the room of a dungeon its venture marker is on.
    ventureRoom :: Maybe RoomIndex.RoomIndex,
    -- | CR 607.2d: Object.chosenColors, less a secret choice (PlayerChoice.SecretChooser) the seat is not privy to (CR 724.4).
    chosenColors :: Map.Map Acquisition.Acquisition (Set.Set Color.Color),
    -- | CR 607.2d: Object.chosenColorless, less a secret choice the seat is not privy to.
    chosenColorless :: Map.Map Acquisition.Acquisition (),
    -- | CR 607.2d: Object.chosenSubtype, less a secret choice the seat is not privy to.
    chosenSubtype :: Map.Map Acquisition.Acquisition (NonEmpty.NonEmpty Subtype.Subtype),
    -- | CR 607.2d: Object.chosenCardType, less a secret choice the seat is not privy to.
    chosenCardType :: Map.Map Acquisition.Acquisition CardType.CardType,
    -- | CR 607.2d: Object.chosenNames, less a secret choice the seat is not privy to.
    chosenNames :: Map.Map Acquisition.Acquisition (Map.Map PlayerId.PlayerId (Set.Set CardName.CardName)),
    -- | CR 607.2d: Object.chosenPlayers, a secret choice's chooser included.
    chosenPlayers :: Map.Map Acquisition.Acquisition (Map.Map PlayerChoice.PlayerChoice PlayerId.PlayerId),
    -- | CR 607.2d: Object.chosenNumber, less a secret choice the seat is not privy to.
    chosenNumber :: Map.Map Acquisition.Acquisition Natural.Natural,
    -- | CR 607.2d: Object.chosenObjects, less a secret choice the seat is not privy to.
    chosenObjects :: Map.Map Acquisition.Acquisition (Set.Set ObjectId.ObjectId)
  }
  deriving (Eq, Show)
