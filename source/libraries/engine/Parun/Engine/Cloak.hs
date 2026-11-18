-- | CR 701.58a, cloak: "turn it face down. It becomes a 2\/2 face-down creature
-- card with ward {2}, no name, no subtypes, and no mana cost. Put that card onto
-- the battlefield face down", and the whole of the keyword action.
--
-- Parun.Engine.Populate's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so the procedure
-- lives in the engine rather than in card data. The closed\/open invariant
-- forbids the rules core casing on an EFFECT's identity, and nothing here does
-- -- Parun.Engine.Resolve.Arms.Counters's Effect.Cloak arm calls in without saying which
-- effect it is.
--
-- The LISTING is the rule's and never a card's, which is the whole reason this
-- is an opcode rather than an Effect.MoveToZone carrying the rider a manifest
-- card writes: CR 701.58a fixes the 2\/2 and the ward {2}, so a printing has
-- nothing to say about them and no card should be able to say it wrong.
--
-- Rule 701.58 has no "whenever a player cloaks", so there is no GameEvent here
-- and no trigger condition to hang one on. Scryfall @o:"cloaks"@, 2026-09-18,
-- returns no card; a printing worded that way is what would need one.
module Parun.Engine.Cloak where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Parun.Types.FaceDownReason as FaceDownReason
import qualified Parun.Types.FaceDownState as FaceDownState
import Parun.Types.Game (Game)
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import Parun.Types.PlayerId (PlayerId)
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.Zone as Zone

-- | CR 701.58a's second and third sentences as one rider: the 2\/2 with ward {2}
-- CR 702.168b lists in the same words (FaceDownCharacteristics.disguisedValue),
-- under a reason of this rule's own so that CR 701.58b's procedure -- and not
-- disguise's -- is the one the permanent is turnable by.
--
-- A settled count rather than a Quantity, Parun.Engine.Foretell.riders' reason:
-- the rule states no counters, so there is no CR 608.2h moment to evaluate one
-- at.
riders :: EntryRiders.EntryRiders Natural ability
riders =
  EntryRiders.defaultValue
    { EntryRiders.faceDown =
        Just
          FaceDownState.MkFaceDownState
            { FaceDownState.reason = FaceDownReason.Cloaked,
              FaceDownState.listed = FaceDownCharacteristics.disguisedValue,
              FaceDownState.turnsUpBefore = Set.empty
            }
    }

-- | CR 701.58a performed by the player on the card: the top card of a library
-- ("cloak the top card of your library", Etrata, Deadly Fugitive's "of that
-- player's library"), a card from a hand (Vannifar, Evolved Enigma), one of a
-- looked-at group (Hide in Plain Sight) or an exiled one (Expose the Culprit).
--
-- NO PROMPT, because the rule leaves nothing to ask: which card is cloaked is
-- the caller's, and the listing is the rule's.
--
-- Through Event.changeZoneEntering, the door the faceDown rider is read from, so
-- CR 708.3 turns the card over BEFORE the move and no reader ever sees it face
-- up on the battlefield. That door is also CR 701.58f: a rule or effect that
-- prohibits the face-down object from entering leaves the move cancelled, and
-- the card is then still the card it was, where it was.
--
-- The permanent enters under the player who cloaked, which is CR 110.2a: that
-- player is the one the effect instructed, whoever's library the card came
-- off. The scenario
-- etrata-deadly-fugitive-cloaks-the-top-card-of-the-damaged-opponent-s-library-under-her
-- proves it.
--
-- The result is the permanent the cloak put onto the battlefield, none when the
-- move was cancelled or replaced elsewhere, for the arm's slot.
cloak :: Settle.Settle -> PlayerId -> ObjectId -> Game [ObjectId]
cloak settle cloaker card = do
  entered <- Event.changeZoneEntering settle card Zone.Battlefield LibraryPosition.defaultValue riders (Just cloaker)
  after <- State.get
  let onBattlefield oid = fmap Object.zone (Game.lookupObject oid after) == Just Zone.Battlefield
  pure (filter onBattlefield (Foldable.toList entered))
