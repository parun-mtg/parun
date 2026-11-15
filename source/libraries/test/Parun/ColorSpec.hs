{-# LANGUAGE GADTs #-}

-- Covers: Parun.Engine.Projection (an object's CR 613 layer-5 colour, including CR
-- 702.114a devoid PRINTED, GRANTED by a static ability and GRANTED by a
-- resolution -- CR 604.3a routes the first apart from the other two, and
-- Slaughter Drone, Slivdrazi Monstrosity and Synthetic Colorless Blessing are the
-- cards that separate them -- CR 613.3's characteristic-defining-ability-first
-- ordering within layer 5, and CR 111.3 token colour),
-- Parun.Engine.Target (the "target nonblack creature" filter
-- below, and Red Elemental Blast's two colour-filtered pools read straight off
-- the card), the P3a colour gates (Doom Blade, Crimson
-- Wisps, Aphotic Wisps, Bad Moon, Dragon Fodder) and this phase's own CR 613.3
-- gates (Indigo Faerie, Red Elemental Blast), plus the CR
-- 608.2b colour-change fizzle and CR 105.3's "become colorless" half (Moonlace,
-- Ersatz Gnomes -- the only cards here that recolour a SPELL by targeting it,
-- and the pair that pins Duration.Indefinite against Duration.UntilEndOfTurn).
-- Mostly gameplay-level: a card is cast or resolved through the stack and the
-- resulting game state is asserted on. The rest assert on a projection over a
-- hand-built board, either because no card in the pool produces the effect
-- (S.withEffect) or because the claim is about a layer BOUNDARY and only
-- projectUpTo can see one ("devoid applies at the START of layer 5").
module Parun.ColorSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Card as Card
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Mint as Mint
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Engine.Target as Target
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Activator as Activator
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.ChangeMode as ChangeMode
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Copyability as Copyability
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.Layer as Layer
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.Modification as Modification
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.ProjectedCharacteristics as PC
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.StackObjectKind as StackObjectKind
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.TargetSlot as TargetSlot

-- "target nonblack creature", the target slot Doom Blade and the CR 115.1a cases share.
nonblackCreature :: TargetSlot.TargetSlot
nonblackCreature = TargetSlot.required Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))

destroyMode :: ModeIndex.ModeIndex
destroyMode = ModeIndex.MkModeIndex 1

-- The one TargetSlot a mode of a printing's spell declares, read out of the
-- JSON-loaded card BY INDEX -- so a swapped mode order is as visible to a
-- legality assertion here as a wrong pool or a wrong filter is. Nothing when the
-- index is out of range or the mode does not declare exactly one slot; the
-- callers assert on that rather than defaulting to a hand-built target slot.
modeTargetSlot :: Printing.Printing -> ModeIndex.ModeIndex -> Maybe TargetSlot.TargetSlot
modeTargetSlot printing idx = case fmap Map.elems (Card.modeTargetSlots idx (S.combinedFace printing)) of
  Just [only] -> Just only
  _ -> Nothing

-- The single activated ability of a printing (Indigo Faerie has exactly one).
-- Same shape as ActivateSpec.theAbility -- duplicated per this test suite's
-- existing convention of group-local helpers (ActivateSpec, ReplacementSpec)
-- rather than centralizing a helper this small in Support.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] [] Nothing (Mint.oneModeOf Map.empty Seq.empty) [] (Activator.Related PlayerRelation.You) Nothing Nothing Nothing Copyability.Copyable Nothing

permanentAbility :: Int
permanentAbility = 1

-- One activated ability of a printing, BY INDEX -- `theAbility` above reads "the
-- single one", which Ersatz Gnomes' two do not fit. Nothing when the index is
-- out of range; the callers assert on that rather than falling back to an
-- ability that would make their assertions vacuous.
abilityAt :: Int -> Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
abilityAt i p = case drop i (Face.activatedAbilities (S.combinedFace p)) of
  ab : _ -> Just ab
  [] -> Nothing

-- Aims every ChooseTargets slot at one object, deferring the rest to
-- S.identityAnswer -- ProjectionSpec.aimAtObject's shape, duplicated because
-- Indigo Faerie's "target permanent" (Pool.Permanents) answers with ToObject.
aimAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- Casts Red Elemental Blast: chooses mode `idx` at CR 700.2's mode prompt and
-- aims every target slot at `oid` -- both of the card's pools (Pool.Spells and
-- Pool.Permanents) answer with ToObject. ModalSpec.chooseModeAt's shape,
-- duplicated per this suite's group-local-helper convention rather than
-- centralized.
blasting :: ModeIndex.ModeIndex -> ObjectId.ObjectId -> Prompt.Prompt r -> r
blasting idx oid p = case p of
  Prompt.ChooseModes {} -> Seq.singleton idx
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Color" $ do
  Spec.it s "CR 202.2 a mono-black card's colour is black" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, gs) = S.addPermanent typhoidRats S.alice gs0
    Spec.assertEq s (Projection.colorsOf ratsId gs) $ Set.singleton Color.Black

  Spec.it s "CR 202.2 a generic-plus-red cost is red, and generic contributes nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, gs) = S.addPermanent piker S.alice gs0
    Spec.assertEq s (Projection.colorsOf pikerId gs) $ Set.singleton Color.Red

  Spec.it s "CR 202.2b an object with no coloured mana symbols is colourless" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let gs0 = Setup.emptyGame S.bothPlayers
        (myrId, gs) = S.addPermanent darksteelMyr S.alice gs0
    Spec.assertEq s (Projection.colorsOf myrId gs) Set.empty

  Spec.it s "CR 202.1b a land has no mana cost, so it is colourless" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs0 = Setup.emptyGame S.bothPlayers
        (mtnId, gs) = S.addPermanent mountain S.alice gs0
    Spec.assertEq s (Projection.colorsOf mtnId gs) Set.empty

  Spec.it s "CR 702.114a devoid makes an object colourless despite a black mana cost" $ do
    -- THE FALSIFIER for "an object's colours are the coloured symbols in its
    -- mana cost": this card's cost is {1}{B} and it is colourless.
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, gs) = S.addPermanent slaughterDrone S.alice gs0
    Spec.assertEq s (Projection.colorsOf droneId gs) Set.empty

  Spec.it s "CR 105.3 a new colour REPLACES all previous colours" $ do
    -- THE FALSIFIER for implementing "becomes red" as an ADD: the Rats are
    -- black, and after the effect they are red and NOT black.
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addPermanent typhoidRats S.alice gs0
        gs = S.withEffect ratsId (Modification.Colors ChangeMode.Replace (Set.singleton Color.Red)) board
    Spec.assertEq s (Projection.colorsOf ratsId gs) $ Set.singleton Color.Red

  Spec.it s "CR 613.3 within layer 5 the CR 702.114a devoid CDA applies first, then the timestamped colour change" $ do
    -- Both live in layer 5 (CR 613.1e). CR 613.3 orders them: devoid, the
    -- characteristic-defining ability, clears the colours at the start of the
    -- layer, and the stored Colors Replace then applies in timestamp order on top --
    -- so the drone ends up black, not colourless.
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, board) = S.addPermanent slaughterDrone S.alice gs0
        gs = S.withEffect droneId (Modification.Colors ChangeMode.Replace (Set.singleton Color.Black)) board
    Spec.assertEq s (Projection.colorsOf droneId gs) $ Set.singleton Color.Black

  Spec.it s "CR 604.3 Transguild Courier is all colors despite its {4}, in hand and on the battlefield" $ do
    courier <- S.printingOf s registry "Transguild Courier"
    let gs0 = Setup.emptyGame S.bothPlayers
        (inHand, handId) = S.handOne courier gs0
        (courierId, gs) = S.addPermanent courier S.alice inHand
    Spec.assertEqWith s "in hand" (Projection.colorsOf handId gs) $ Set.fromList [minBound .. maxBound]
    Spec.assertEqWith s "on the battlefield" (Projection.colorsOf courierId gs) $ Set.fromList [minBound .. maxBound]

  Spec.it s "CR 604.3 Bad Moon pumps Transguild Courier, which is black among all colors" $ do
    badMoon <- S.printingOf s registry "Bad Moon"
    courier <- S.printingOf s registry "Transguild Courier"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addPermanent badMoon S.alice gs0
        (courierId, gs) = S.addPermanent courier S.alice withMoon
    Spec.assertEqWith s "the 3/3 Courier is 4/4" (Projection.powerOf courierId gs, Projection.toughnessOf courierId gs) (Just 4, Just 4)

  Spec.it s "CR 613.3 a timestamped colour change applies over Transguild Courier's all colors" $ do
    courier <- S.printingOf s registry "Transguild Courier"
    let gs0 = Setup.emptyGame S.bothPlayers
        (courierId, board) = S.addPermanent courier S.alice gs0
        gs = S.withEffect courierId (Modification.Colors ChangeMode.Replace (Set.singleton Color.Red)) board
    Spec.assertEq s (Projection.colorsOf courierId gs) $ Set.singleton Color.Red

  Spec.it s "Bad Moon pumps a black creature but not a red one" $ do
    badMoon <- S.printingOf s registry "Bad Moon"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addPermanent badMoon S.alice gs0
        (ratsId, withRats) = S.addPermanent typhoidRats S.alice withMoon
        (pikerId, gs) = S.addPermanent piker S.alice withRats
    Spec.assertEqWith s "the black Rats are 2/2" (Projection.powerOf ratsId gs) $ Just 2
    Spec.assertEqWith s "the red Piker is unchanged at 2" (Projection.powerOf pikerId gs) $ Just 2
    Spec.assertEqWith s "the red Piker's toughness is unchanged at 1" (Projection.toughnessOf pikerId gs) $ Just 1

  Spec.it s "CR 702.114a Bad Moon does not pump a devoid creature with a black mana cost" $ do
    -- FALSIFIER, reader (b) half: a naive "colours are the mana cost's
    -- symbols" implementation pumps this 2/2 to 3/3.
    badMoon <- S.printingOf s registry "Bad Moon"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addPermanent badMoon S.alice gs0
        (droneId, gs) = S.addPermanent slaughterDrone S.alice withMoon
    Spec.assertEqWith s "power unchanged" (Projection.powerOf droneId gs) $ Just 2
    Spec.assertEqWith s "toughness unchanged" (Projection.toughnessOf droneId gs) $ Just 2

  Spec.it s "CR 613 a layer-5 colour change moves a creature INTO Bad Moon's set" $ do
    badMoon <- S.printingOf s registry "Bad Moon"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addPermanent badMoon S.alice gs0
        (pikerId, board) = S.addPermanent piker S.alice withMoon
        gs = S.withEffect pikerId (Modification.Colors ChangeMode.Replace (Set.singleton Color.Black)) board
    Spec.assertEqWith s "the now-black Piker is 3/2" (Projection.powerOf pikerId gs) $ Just 3

  Spec.it s "CR 115.1a a black creature is not a legal 'target nonblack creature'" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, withRats) = S.addPermanent typhoidRats S.alice gs0
        (pikerId, gs) = S.addPermanent piker S.alice withRats
        legal = Target.legalRecipients StackObjectKind.Spell Nothing S.noSource nonblackCreature gs
    Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "the red Piker is legal"
    Spec.assertBool s (not (Set.member (Recipient.ToCreature ratsId) legal)) "the black Rats are not"

  Spec.it s "CR 702.114a a devoid creature with a black cost IS a legal nonblack target" $ do
    -- FALSIFIER, reader (a) half.
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, gs) = S.addPermanent slaughterDrone S.alice gs0
        legal = Target.legalRecipients StackObjectKind.Spell Nothing S.noSource nonblackCreature gs
    Spec.assertBool s (Set.member (Recipient.ToCreature droneId) legal) "colourless is nonblack"

  Spec.it s "CR 105.3 2008-05-01 a colour change overwrites ALL previous colours, even a multicoloured one" $ do
    -- Gatherer ruling on Crimson Wisps / Aphotic Wisps (WotC, 2008-05-01):
    -- "An effect that changes a permanent's colors overwrites all its old
    -- colors unless it specifically says 'in addition to its other
    -- colors.' ... It doesn't matter what colors it used to be (even if,
    -- for example, it used to be blue and black)." Transcribed with two
    -- stacked Colors Replace effects rather than a card, since no card in the
    -- pool is printed multicoloured: the later effect leaves no residue
    -- of either colour the earlier one set, not just the printed colour.
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addPermanent typhoidRats S.alice gs0
        multi = S.withEffect ratsId (Modification.Colors ChangeMode.Replace (Set.fromList [Color.Blue, Color.Black])) board
        gs = S.withEffect ratsId (Modification.Colors ChangeMode.Replace (Set.singleton Color.Red)) multi
    Spec.assertEqWith s "red only, no residue of blue or black" (Projection.colorsOf ratsId gs) $ Set.singleton Color.Red

  Spec.it s "CR 105.3 an 'in addition' colour effect ADDS rather than replaces" $ do
    -- The falsifier for implementing every colour change as a replacement: the
    -- Rats are black, and after a Colors InAddition they are black AND blue.
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addPermanent typhoidRats S.alice gs0
        gs = S.withEffect ratsId (Modification.Colors ChangeMode.InAddition (Set.singleton Color.Blue)) board
    Spec.assertEq s (Projection.colorsOf ratsId gs) $ Set.fromList [Color.Black, Color.Blue]

  Spec.it s "CR 613.1c/613.1e 2008-05-01 changing a permanent's colour doesn't change its text" $ do
    -- Gatherer ruling on Crimson Wisps / Aphotic Wisps (WotC, 2008-05-01):
    -- "Changing a permanent's color won't change its text. If you turn
    -- Wilt-Leaf Liege blue, it will still affect green creatures and
    -- white creatures." Transcribed with Bad Moon, whose own text
    -- ("Black creatures get +1/+1") is keyed to its OWN printed colour:
    -- turn Bad Moon itself red with a stored Colors Replace effect (S.withEffect
    -- reaches non-creature permanents, which no card in the pool
    -- targets) and its ability still reads Typhoid Rats as black. This
    -- guards CR 613.1e/613.1c's layer separation -- colour is layer 5,
    -- text-changing is layer 3 -- and must never be implemented as a
    -- colour change rewriting the object's own text.
    badMoon <- S.printingOf s registry "Bad Moon"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (moonId, withMoon) = S.addPermanent badMoon S.alice gs0
        (ratsId, board) = S.addPermanent typhoidRats S.alice withMoon
        gs = S.withEffect moonId (Modification.Colors ChangeMode.Replace (Set.singleton Color.Red)) board
    Spec.assertEqWith s "Bad Moon itself is now red" (Projection.colorsOf moonId gs) $ Set.singleton Color.Red
    Spec.assertEqWith s "the black Rats are still pumped to 2 power" (Projection.powerOf ratsId gs) $ Just 2
    Spec.assertEqWith s "the black Rats are still pumped to 2 toughness" (Projection.toughnessOf ratsId gs) $ Just 2

  Spec.it s "Indigo Faerie's 'in addition' blue makes a devoid drone a legal Red Elemental Blast target" $ do
    -- Slaughter Drone is devoid, so it is colourless until something adds a
    -- colour. Red Elemental Blast's destroy mode reads blue, and the drone is
    -- outside its set until Indigo Faerie's ability resolves. Both halves are
    -- driven through real cards -- a real activation and a real cast, not
    -- S.withEffect and not a hand-built TargetSlot -- so the two JSON files, not
    -- just the projection, are under test.
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    indigoFaerie <- S.printingOf s registry "Indigo Faerie"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    redElementalBlast <- S.printingOf s registry "Red Elemental Blast"
    let base = S.landsInPlay island 1
        -- The Island pays Indigo Faerie's {U}; the Mountain pays the blast's {R}.
        (_, withMountain) = S.addPermanent mountain S.alice base
        (faerieId, withFaerie) = S.addPermanent indigoFaerie S.alice withMountain
        (droneId, board) = S.addPermanent slaughterDrone S.alice withFaerie
        (gs, rebId) = S.handOne redElementalBlast board
        activated = snd (Engine.runGamePure (aimAtObject droneId) gs (Activate.activateAbility S.alice faerieId (theAbility indigoFaerie)))
        blued = snd (Engine.runGamePure (aimAtObject droneId) activated Stack.resolveTop)
        answer :: Prompt.Prompt r -> r
        answer = blasting destroyMode droneId
        cast = snd (Engine.runGamePure answer blued (S.cast S.alice rebId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "colourless before" (Projection.colorsOf droneId gs) Set.empty
    Spec.assertEqWith s "blue after" (Projection.colorsOf droneId blued) $ Set.singleton Color.Blue
    case modeTargetSlot redElementalBlast destroyMode of
      Nothing -> Spec.assertFailure s "Red Elemental Blast's destroy mode declares exactly one target slot"
      Just destroySlot -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToObject droneId) (Target.legalRecipients StackObjectKind.Spell Nothing S.noSource destroySlot gs)))
          "the colourless drone is not a legal 'target blue permanent'"
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject droneId) (Target.legalRecipients StackObjectKind.Spell Nothing S.noSource destroySlot blued))
          "the blue drone is"
    Spec.assertBool s (not (Set.member droneId (GameState.battlefield after))) "and Red Elemental Blast destroys it"
    Spec.assertBool s (Set.member faerieId (GameState.battlefield after)) "the blast hit the drone, not the equally blue Faerie"

  Spec.it s "CR 613.3 devoid applies at the START of layer 5, so layers 2-4 read the mana cost" $ do
    -- CR 613.3 applies characteristic-defining abilities first WITHIN a layer,
    -- and devoid's layer is 5 (CR 613.1e). A layer-2, -3 or -4 effect whose
    -- affected set is colour-keyed therefore sees the mana cost's black, not
    -- the colourless devoid produces. Seeding devoid gets this wrong (#35).
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, gs) = S.addPermanent slaughterDrone S.alice gs0
        cands = Projection.gather gs
        below = Projection.projectUpTo Layer.Color cands droneId gs
    Spec.assertEqWith s "black below layer 5" (PC.colors below) $ Set.singleton Color.Black
    Spec.assertEqWith s "colourless once layer 5 has applied" (Projection.colorsOf droneId gs) Set.empty

  Spec.it s "CR 604.3a a GRANTED devoid makes its own five-colour source colourless" $ do
    -- Slivdrazi Monstrosity (Mystery Booster Playtest Cards 2021, cmb2 #102) is
    -- the pool's only card whose STATIC ability grants devoid: "Slivers you
    -- control have devoid and annihilator 1." (Synthetic Colorless Blessing below
    -- grants it by a resolution instead.) It is itself a Sliver Eldrazi, so its
    -- own ability reaches it, and its mana cost is {C}{W}{U}{B}{R}{G} -- five
    -- colours for the grant to clear.
    --
    -- The granted instance is NOT a characteristic-defining ability: CR 604.3a
    -- requires all five clauses, and this ability fails (2) -- it is not printed
    -- on the OTHER Slivers it affects -- as well as (3), since it directly
    -- affects other objects, and (4) for the source itself, an ability an object
    -- grants to itself. So it is an ordinary colour-changing effect in layer 5
    -- (CR 613.1e), applied in timestamp order rather than at the start of the
    -- layer.
    --
    -- Not transcribed: the same ability's annihilator 1, CR 702.86a's attack
    -- trigger, which Parun.Types.Keyword cannot name (#794).
    slivdrazi <- S.printingOf s registry "Slivdrazi Monstrosity"
    let gs0 = Setup.emptyGame S.bothPlayers
        (slivId, gs) = S.addPermanent slivdrazi S.alice gs0
        cands = Projection.gather gs
        below = Projection.projectUpTo Layer.Color cands slivId gs
    Spec.assertEqWith s "all five colours below layer 5" (PC.colors below) $
      Set.fromList [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green]
    Spec.assertEqWith s "colourless once layer 5 has applied" (Projection.colorsOf slivId gs) Set.empty

  Spec.it s "CR 702.114a Bad Moon does not pump a creature whose devoid was GRANTED" $ do
    -- The READER half: a granted devoid has to be visible to the rules, not only
    -- to Projection.colorsOf. Bad Moon's affected set is "black creatures", read
    -- at layer 7c against a projection that has already applied layer 5, and
    -- Slivdrazi's mana cost has a {B} in it. Its own granted devoid is what keeps
    -- the 8/8 an 8/8 instead of a 9/9.
    badMoon <- S.printingOf s registry "Bad Moon"
    slivdrazi <- S.printingOf s registry "Slivdrazi Monstrosity"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addPermanent badMoon S.alice gs0
        (slivId, gs) = S.addPermanent slivdrazi S.alice withMoon
    Spec.assertEqWith s "power unchanged" (Projection.powerOf slivId gs) $ Just 8
    Spec.assertEqWith s "toughness unchanged" (Projection.toughnessOf slivId gs) $ Just 8

  Spec.it s "Ersatz Gnomes' second ability makes a permanent colourless only until end of turn" $ do
    -- The same colour change as Moonlace's, with the OTHER duration, so this and
    -- data/scenarios/color's Moonlace scenario together pin Duration.Indefinite and Duration.UntilEndOfTurn to the
    -- right cards. CR 514.2's cleanup sweep is what ends this one, and Bad Moon
    -- reads the revert: the Rats are 2/2, then 1/1, then 2/2 again.
    badMoon <- S.printingOf s registry "Bad Moon"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    ersatzGnomes <- S.printingOf s registry "Ersatz Gnomes"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addPermanent badMoon S.alice gs0
        (ratsId, withRats) = S.addPermanent typhoidRats S.alice withMoon
        (gnomesId, gs) = S.addPermanent ersatzGnomes S.alice withRats
    case abilityAt permanentAbility ersatzGnomes of
      Nothing -> Spec.assertFailure s "Ersatz Gnomes prints two activated abilities"
      Just ability -> do
        let answer :: Prompt.Prompt r -> r
            answer = aimAtObject ratsId
            activated = snd (Engine.runGamePure answer gs (Activate.activateAbility S.alice gnomesId ability))
            after = snd (Engine.runGamePure answer activated Stack.resolveTop)
            afterCleanup = snd (Engine.runGamePure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
        Spec.assertEqWith s "before: black, 2/2 under Bad Moon" (Projection.colorsOf ratsId gs, Projection.powerOf ratsId gs) (Set.singleton Color.Black, Just 2)
        Spec.assertEqWith s "after: colourless, 1/1" (Projection.colorsOf ratsId after, Projection.powerOf ratsId after) (Set.empty, Just 1)
        Spec.assertEqWith s "CR 514.2 drops the effect at cleanup" (StoredEffects.continuousEffects (GameState.stored afterCleanup)) []
        Spec.assertEqWith s "black and 2/2 again" (Projection.colorsOf ratsId afterCleanup, Projection.powerOf ratsId afterCleanup) (Set.singleton Color.Black, Just 2)
