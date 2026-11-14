-- Every slot NAME a Parun.Types.Quantity carries: the reads and the rename,
-- each Parun.Engine.Children.quantityChildren run with the visitors it has an
-- opinion about. Structural only -- nothing here touches a GameState, which
-- Parun.Engine.Quantity's evaluation does.
--
-- A module of its own for a MODULE CYCLE rather than for cohesion.
-- Parun.Engine.Modal is CR 700.2d's per-occurrence rename and sits below
-- Parun.Engine.Card, which Parun.Engine.Game imports; Parun.Engine.Quantity and
-- Parun.Engine.Count both read a board and so sit above Game. Splitting the walk
-- out is the repo's parametric-polymorphism escape one step further along: the
-- rename is a function of the syntax alone, so it belongs where the syntax is.
module Parun.Engine.QuantitySlot where

import qualified Data.Functor.Const as Const
import qualified Data.Functor.Identity as Identity
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Parun.Engine.Children as Children
import qualified Parun.Types.AttackingPlayers as AttackingPlayers
import qualified Parun.Types.Count as Count.Type
import qualified Parun.Types.InZone as InZone
import qualified Parun.Types.PlayerRef as PlayerRef
import Parun.Types.Quantity (Quantity)
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Scope as Scope
import Parun.Types.SlotName (SlotName)

-- The binding slots a quantity READS. The read half of the dataflow lint whose
-- write half is Resolve.definedSlots -- so a card whose "for each ... destroyed
-- this way" names a slot nothing binds is a failing test, not a silent no-op.
--
-- Binding.variableX is reported like any other slot, which is what #14 bought:
-- the "reads X iff the cost declares {X}" lint is then just the ordinary
-- available-slots comparison, because Parun.CardSpec puts variableX on the
-- AVAILABLE side exactly when the cost prints an {X}. No arm here has to know
-- that X is special, and nothing subtracts it -- the fact lives where it belongs,
-- in what casting makes available.
--
-- Every slot Parun.Engine.Children's slot visitor reaches: the amount readers
-- and AgainstSlot's object. A PlayerRef's slots name a TARGET slot rather than
-- an amount, so `slots` leaves them to Resolve.slotsOf, and nestedRefs below is
-- what reports them.
slots :: Quantity -> Set SlotName
slots =
  Const.getConst
    . Children.quantityChildren
      (Children.descendingQuantities (Const.Const . slots))
        { Children.slot = \_arity -> Const.Const . Set.singleton
        }

-- Every slot NAME a quantity carries, rewritten -- `slots`' half above
-- and the target slots its nested references name, which nestedRefs below is the
-- reading counterpart of. CR 700.2d's per-occurrence rename is the caller
-- (Parun.Engine.Modal.instanceScope): a target slot's CR 202.3 computed bound may
-- name a SIBLING slot of the same mode, so a mode chosen twice would otherwise
-- judge occurrence 1's bound against occurrence 0's answer. The function it is
-- given is a PARTIAL rename (Modal.ownSlot), so a name the mode does not declare
-- comes back unchanged.
--
-- The Scope says whose zone, which players or which bound slot, and a nested
-- quantity may hide a reference of its own. Proved by Parun.TargetSpec's "CR
-- 700.2d a repeated mode's computed bound measures its own occurrence's
-- sibling slot".
renameSlots :: (SlotName -> SlotName) -> Quantity -> Quantity
renameSlots rename =
  let go =
        Identity.runIdentity
          . Children.quantityChildren
            (Children.descendingQuantities (Identity.Identity . go))
              { Children.slot = \_arity -> Identity.Identity . rename,
                Children.player = Identity.Identity . renamePlayerRef rename,
                Children.scope = Identity.Identity . renameScope rename . mapScope (renamePlayerRef rename)
              }
   in go

-- Every read of a slot this quantity makes that `slots` above does not report: a
-- PlayerRef nested inside it, which names a TARGET slot rather than an amount one
-- and which that function leaves to Resolve.Slots.quantitySlots, the fold that
-- carries it to the D4 dataflow lint -- plus CR 400.7j's Scope.OverBound,
-- which names a slot outright. `Left` is a reference, whose ARITY only the reader
-- knows (Resolve.playerRefSlots); `Right` is a slot named directly.
--
-- Two callers want different halves of it, which is why this is the set of reads
-- rather than a Bool: Parun.Engine.Quantity.slotsAreExhaustive asks whether any of
-- them names a slot at all, and Resolve.targetSlotSlots turns them into the slots a
-- CR 202.3 computed bound reads, so the D4 dataflow lint can see a bound naming a
-- slot its carrier never binds.
nestedRefs :: Quantity -> Set (Either PlayerRef.PlayerRef SlotName)
nestedRefs =
  Const.getConst
    . Children.quantityChildren
      (Children.descendingQuantities (Const.Const . nestedRefs))
        { Children.player = Const.Const . Set.singleton . Left,
          Children.scope = Const.Const . scopeRefs
        }

-- Every Count reachable from a Quantity: a leaf Count directly, plus the ones
-- nested through the arms that compose one (CR 208.2's printed 1+*, CR 107.1a's
-- rounding, a negation, an aggregation's per-member number) and through
-- AgainstSlot's redirected payload.
--
-- ONE enumeration for two readers that must not disagree about where a Count
-- sits: Resolve.Slots.quantitySlots reads each Count's FILTER, which is card text
-- naming slots like any other (a Filter.IsBound under a fold is a read of the
-- resolving object's bindings), and Parun.CardSpec's Count sweeps ask the same
-- Counts about their scopes and counter kinds.
nestedCounts :: Quantity -> [Count.Type.Count Quantity]
nestedCounts quantity =
  let own = case quantity of
        Quantity.Count c -> [c]
        _ -> []
   in own <> Const.getConst (Children.quantityChildren (Children.descendingQuantities (Const.Const . nestedCounts)) quantity)

-- A scope's own read. Every scope that names players takes a PlayerRef and CR
-- 608.2i's look-back names nothing, so the same question as the arms above --
-- except CR 400.7j's fold, which names a slot rather than a reference.
--
-- The OverBound arm's read is PROVEN rather than fenced: Parun.CardSpec's "the
-- lint itself catches a computed bound naming a slot through a player" plants a
-- bound scoped this way and asserts the slot is reported.
scopeRefs :: Scope.Scope -> Set (Either PlayerRef.PlayerRef SlotName)
scopeRefs scope = case scope of
  Scope.InZone (InZone.MkInZone _ ref) -> Set.singleton (Left ref)
  Scope.InHistory _ -> Set.empty
  Scope.OverPlayers ref -> Set.singleton (Left ref)
  Scope.OverBound _ slot -> Set.singleton (Right slot)
  Scope.InPosition _ (InZone.MkInZone _ ref) -> Set.singleton (Left ref)
  Scope.AsTurnBegan -> Set.empty

-- Every slot NAME renameSlots above would rewrite, as a set: `slots`' amount half
-- plus the target slots the nested references name. The READING partner of that
-- rename, and the one function a caller wants when the question is "which of this
-- announcement's own slots does this bound depend on" --
-- Parun.Engine.Target.jointlyJudged and legalSetsGiven both ask it, and
-- Parun.Engine.Resolve.Slots.targetSlotSlots asks the same question with arities.
allSlots :: Quantity -> Set SlotName
allSlots quantity = Set.union (slots quantity) (refSlots quantity)

-- allSlots' second half on its own: the slots nestedRefs finds, with each
-- PlayerRef turned into the slots it names. renamePlayerRef's reading partner
-- through playerRefSlots, arm for arm.
refSlots :: Quantity -> Set SlotName
refSlots = foldMap (either playerRefSlots Set.singleton) . nestedRefs

-- The slots one reference names -- renamePlayerRef's reading partner, derived
-- from the same traversal so the two cannot disagree about which arms name one.
-- Parun.Engine.Resolve.Slots.playerRefSlots is the third reader of this classification
-- and adds the ARITY each read has, which no rename needs.
playerRefSlots :: PlayerRef.PlayerRef -> Set SlotName
playerRefSlots = Const.getConst . overPlayerRefSlots (Const.Const . Set.singleton)

-- One reference's slots, renamed. CR 700.2d's rename reaches a PlayerRef because
-- a target slot's CR 202.3 computed bound may read one ("where X is the amount of
-- life that player gained this turn"), and "that player" is then a sibling slot of
-- the same mode.
renamePlayerRef :: (SlotName -> SlotName) -> PlayerRef.PlayerRef -> PlayerRef.PlayerRef
renamePlayerRef rename = Identity.runIdentity . overPlayerRefSlots (Identity.Identity . rename)

-- Every slot NAME a reference carries, as one traversal -- the two functions above
-- are its Const and Identity instances. Exhaustive, bakePlayerRef's posture: a new
-- reference naming a slot must answer here rather than keep a printed name.
overPlayerRefSlots :: (Applicative f) => (SlotName -> f SlotName) -> PlayerRef.PlayerRef -> f PlayerRef.PlayerRef
overPlayerRefSlots f ref = case ref of
  PlayerRef.InSlot slot -> fmap PlayerRef.InSlot (f slot)
  PlayerRef.EachInSlot slot -> fmap PlayerRef.EachInSlot (f slot)
  -- The slot decides who is left OUT rather than who is in, which changes nothing
  -- about whose namespace the name is in.
  PlayerRef.EachPlayerExcept slot -> fmap PlayerRef.EachPlayerExcept (f slot)
  -- CR 702.116a's exclusion, the arm above narrowed to opponents and read the
  -- same way.
  PlayerRef.EachOpponentExcept slot -> fmap PlayerRef.EachOpponentExcept (f slot)
  PlayerRef.HolderOfBound holder hop slot -> fmap (PlayerRef.HolderOfBound holder hop) (f slot)
  PlayerRef.PlayerHostOfBound slot -> fmap PlayerRef.PlayerHostOfBound (f slot)
  -- The arm above's namespace, one record over.
  PlayerRef.ChosenPlayerOfBound slot choice -> fmap (`PlayerRef.ChosenPlayerOfBound` choice) (f slot)
  PlayerRef.Attacking attacking -> fmap (\slot -> PlayerRef.Attacking attacking {AttackingPlayers.attacked = slot}) (f (AttackingPlayers.attacked attacking))
  -- The ones that name no slot at all: the table, CR 109.5's relation, a baked
  -- seat or object, and the candidate whichever fold is running supplies.
  PlayerRef.Relative _ -> pure ref
  PlayerRef.Specific _ -> pure ref
  PlayerRef.HolderOfObject _ _ -> pure ref
  PlayerRef.Candidate -> pure ref
  PlayerRef.Superlative _ -> pure ref
  PlayerRef.InRole _ -> pure ref

-- A scope's own slot, renamed -- scopeRefs' Right arm turned around, and paired
-- with it arm for arm. Only CR 400.7j's fold names a slot outright; the players
-- the other arms name are PlayerRefs, which renameSlots rewrites through
-- mapScope instead.
renameScope :: (SlotName -> SlotName) -> Scope.Scope -> Scope.Scope
renameScope rename scope = case scope of
  Scope.OverBound reading slot -> Scope.OverBound reading (rename slot)
  Scope.InZone _ -> scope
  Scope.OverPlayers _ -> scope
  Scope.InHistory _ -> scope
  Scope.InPosition {} -> scope
  Scope.AsTurnBegan -> scope

-- A scope's reference, rewritten. Every scope that names players takes one; CR
-- 608.2i's look-back names none. Shared by bakeBound, forCandidate and
-- renameSlots, so a new scope carrying a reference fails to compile here rather
-- than keep an old one in any of them.
mapScope :: (PlayerRef.PlayerRef -> PlayerRef.PlayerRef) -> Scope.Scope -> Scope.Scope
mapScope f scope = case scope of
  Scope.InZone (InZone.MkInZone zone ref) -> Scope.InZone (InZone.MkInZone zone (f ref))
  Scope.OverPlayers ref -> Scope.OverPlayers (f ref)
  Scope.InHistory _ -> scope
  -- CR 400.7j's fold names a SLOT rather than a player reference, so there is
  -- nothing here for either baking to rewrite.
  Scope.OverBound {} -> scope
  Scope.InPosition position (InZone.MkInZone zone ref) -> Scope.InPosition position (InZone.MkInZone zone (f ref))
  Scope.AsTurnBegan -> scope
