module Parun.Types.BecomeCopy where

import qualified Parun.Types.CopyException as CopyException
import qualified Parun.Types.CopyOriginal as CopyOriginal
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.ObjectRef as ObjectRef

-- | The payload of Parun.Types.Effect's BecomeCopy arm: an effect turning an
-- object that already exists into a copy of another one (CR 707.1), which for a
-- permanent is CR 707.4's change. Unstable Shapeshifter's "this creature becomes
-- a copy of that creature" is @original = OfObject (InSlot became)@,
-- @subject = EachMatching IsSource@.
--
-- NEITHER SIDE is confined to the battlefield: CR 707.1's object is a "spell,
-- permanent, or card", so Synthetic Mirror of the Fallen's subject is a card in
-- a graveyard and Parun.CastSpec's "CR 707.2 a graveyard card that is a copy is
-- priced at the copy's mana cost" is what proves the copiable values follow it
-- there.
--
-- The copy FOLLOWS the card onto the stack, by Bombardment's ruling
-- (2019-11-12): a card stops being a Missile as it "changes zones other than by
-- being cast as a Missile", so CR 601.2a's "all the characteristics of the
-- card" include the copy effect. A stamped copy is announced off the stamp
-- (Parun.Engine.Game.castingFaceOf) and stamped on the spell
-- (Parun.Engine.Cast.carryCopyIntoSpell), which the scenario "CR 707.2 a
-- graveyard card that is a copy is priced at the copy's mana cost" proves; a copy
-- with a DURATION trades the card for the spell in the row's fixed set
-- (Parun.Engine.Cast.followCopyIntoSpell).
--
-- Not implemented: Bombardment's ruling that a Missile keeps its own supertypes
-- (#8555).
--
-- A SPELL on the stack is the third noun in CR 707.1's list: it resolves the
-- copied text (Parun.Engine.Projection.View.spellFaceOf) with the original's
-- choices (CR 707.2, Parun.Engine.Resolve.Common.acquireChoices), which Synthetic
-- Mimicry and Parun.CopySpec's "CR 707.2 a spell that becomes a copy of a Bolt"
-- prove. Transcantation's original is a card NAME (Parun.Types.CopyOriginal),
-- and its subject acquires no choices -- "it stops having any targets".
--
-- TWO fields, named rather than positional for Parun.Types.RequireBlock's
-- reason: the sides are not interchangeable, and a card file that swapped them
-- would decode into a copy pointing the wrong way -- the entrant becoming a copy
-- of the Shapeshifter instead.
--
-- The subject is an ObjectRef rather than a bare SlotName, for the reason
-- CreateCopy's comment gives: it is Unstable Shapeshifter's "this permanent" and
-- so an EachMatching over IsSource, and the Mirror's targeted card and so a
-- slot. Mirrorweave's "each other creature becomes a copy of target nonlegendary
-- creature" is the swept shape the same field takes.
--
-- The DURATION is Nothing for the printings that state none, and those take a
-- different road: the copiable values are STAMPED onto the subject's copy
-- snapshot (Parun.Engine.Binding.setCopy), which is what CR 707.3 asks for --
-- "objects that copy the object will use the new copiable values" -- and a
-- snapshot has nowhere to record an ending. A stated duration (Mirrorweave's
-- "until end of turn") is stored as a Parun.Types.ActiveCopy row instead, which
-- carries the same snapshot plus the expiry that ends it, and which
-- Parun.Engine.Projection.View.stampedSnapshotOf layers over the stamp -- so CR
-- 707.3 holds on both roads and only the stored one can end. A stamp made while
-- a row stands takes its subject out of the row (CR 613.7).
data BecomeCopy card ability = MkBecomeCopy
  { original :: CopyOriginal.CopyOriginal card,
    subject :: ObjectRef.ObjectRef,
    -- | CR 611.2a: how long the copy lasts, absent on a card that states no
    -- duration.
    duration :: Maybe Duration.Duration,
    -- | CR 707.9's "except ..." clause, empty for a copy effect that states none.
    -- The SAME list EntryRewrite.AsCopy carries, and applied by the same fold
    -- (Parun.Engine.Replacement.applyCopyExceptions) into the snapshot this opcode
    -- stamps, which is what CR 707.9a asks for: the gained ability "becomes part
    -- of the copiable values for the copy", so a token copy of the Shapeshifter
    -- taken afterwards has it too (CR 707.2).
    exceptions :: [CopyException.CopyException ability],
    -- | CR 707.10c's "its controller may choose new targets for it"
    -- (Transcantation), asked of each subject on the stack.
    newTargets :: Bool
  }
  deriving (Eq, Ord, Show)
