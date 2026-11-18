{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 702.140's mutate and CR 731's merged permanents: Parun.Types.Keyword's
-- Mutate arm, the target slot and cost Parun.Engine.Keyword mints from it,
-- Parun.Engine.Cast's CR 601.2b stamp and CR 601.2c target, Parun.Engine.Stack's
-- CR 702.140b/702.140c fork, Parun.Engine.Event.merge, Parun.Types.Source's
-- OfMerge arm and the projection read Parun.Engine.Projection.View's
-- withMergedAbilities adds for CR 702.140e. CR 731.2d's token components come
-- through Parun.Types.MergeComponent, whose arms are what a merged permanent's
-- token-ness and CR 731.3's departure each read -- and whose melded arm is what
-- lets a melded permanent be mutated onto at all.
--
-- Cubwarden is the producer: {3}{W} 3\/5 Creature -- Cat, "Mutate {2}{W}{W}",
-- lifelink, "Whenever this creature mutates, create two 1\/1 white Cat creature
-- tokens with lifelink". One existing keyword makes the topmost component's
-- ability observable, and the trigger makes rule 702.140d's own event
-- observable.
--
-- Misthoof Kirin is the FACE-DOWN host CR 731.2e's cases merge with: {2}{W} 2/1
-- Creature -- Kirin, flying, vigilance, "Megamorph {1}{W}". Cast face down for
-- CR 702.37a's {3} it is a nameless 2/2 with no abilities, so every reading of a
-- face-down component differs on it, and CR 702.37e's special action is what
-- turns the merged permanent back over.
--
-- Akki Lavarunner // Tok-Tok, Volcano Born is the FLIP host CR 731.2h needs, and
-- data/cards/'s only Flip printing: a {3}{R} 1/1 Goblin Warrior with haste whose
-- combat damage to an opponent flips it into the legendary 2/2 Goblin Shaman
-- Tok-Tok, Volcano Born. CR 710's own coverage of it is Parun.FlipSpec's.
--
-- Blightreaper Thallid // Blightsower Thallid is the DOUBLE-FACED host CR
-- 731.2i needs, turned over by its own activated ability.
--
-- Synthetic Graftling // Synthetic Graftbeast is the double-faced MUTATE card
-- CR 731.2i's two-component cases need (Scryfall `keyword:mutate is:dfc
-- include:extras`, 2026-10-12, no hit): a {1}{G} 2/2 Beast with "Mutate {G}" and "{2}{G}: Transform this
-- permanent. Activate only as a sorcery.", whose back face is a 4/4 Beast with
-- trample.
--
-- Falcon Abomination is the creature it merges with: {2}{U} 2\/2 Creature --
-- Bird Zombie, flying, "When this creature enters, create a 2\/2 black Zombie
-- creature token with decayed". Non-Human (CR 702.140a), a DIFFERENT name, box
-- and subtypes from Cubwarden (CR 731.2a), one keyword of its own (CR 702.140e)
-- and an enters-the-battlefield trigger whose token is what CR 731.2b's "isn't
-- considered to have just entered the battlefield" is read off. The two token
-- kinds are distinct, so one count can never stand in for the other.
module Parun.MutateSpec where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Card as Card
import qualified Parun.Engine.Cast as Cast
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Departure as Departure
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Extra.Int as Int
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.CandidateCost as CandidateCost
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.CommandZoneDecision as CommandZoneDecision
import qualified Parun.Types.ComponentCard as ComponentCard
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.Departure as Departure.Type
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.FaceDownReason as FaceDownReason
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.MergeComponent as MergeComponent
import qualified Parun.Types.Moved as Moved
import qualified Parun.Types.MutateSide as MutateSide
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.ProjectedCharacteristics as PC
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Protection as Protection
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.ReplacementEntry as ReplacementEntry
import qualified Parun.Types.RevealCause as RevealCause
import qualified Parun.Types.Revealed as Revealed
import qualified Parun.Types.RoomHalf as RoomHalf
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.ShownComponent as ShownComponent
import qualified Parun.Types.Sickness as Sickness
import qualified Parun.Types.Source as Source
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.Transformed as Transformed
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChange as ZoneChange

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Mutate" $ do
  -- CR 708.9's first sentence over CR 731.2e's mix: Cubwarden merges OVER the
  -- face-down Kirin, so the pile is face up and the Kirin component face down,
  -- and alice Murders the pile. She reveals the Kirin component as it moves.
  -- The paired leg merges UNDER, so the pile itself is face down and is
  -- revealed whole.
  Spec.it s "CR 708.9 a face-down component is revealed as its face-up pile is destroyed" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    kirin <- S.printingOf s registry "Misthoof Kirin"
    cubwarden <- S.printingOf s registry "Cubwarden"
    murder <- S.printingOf s registry "Murder"
    case faceDownBoard plains kirin cubwarden of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (host, board, spellId) -> do
        let murdered side =
              let (armed, murderId) = S.handOne murder (S.landsFor swamp S.alice 3 (merging side host board spellId))
               in S.runPure (mutatingAt side host) armed (S.cast S.alice murderId >> Monad.replicateM_ 3 (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)
            over = murdered MutateSide.Over
            under = murdered MutateSide.Under
        Spec.assertEqWith s "CR 708.9 alice revealed the face-down Kirin component as the face-up pile left" (departureReveals over) [(S.alice, host, RevealCause.LeavingFaceDownComponent)]
        Spec.assertEqWith s "CR 708.9 merged under, the pile is face down and revealed whole" (departureReveals under) [(S.alice, host, RevealCause.LeavingFaceDown)]
        Spec.assertEqWith s "CR 701.20a the reveal shows the Kirin card, neither the nameless 2/2 nor the Cubwarden pile over it" (departureRevealNames over) [Set.singleton (CardName.MkCardName (Text.pack "Misthoof Kirin"))]
        Spec.assertEqWith s "CR 701.20a the face-down pile is shown face up, its topmost Kirin's name" (departureRevealNames under) [Set.singleton (CardName.MkCardName (Text.pack "Misthoof Kirin"))]
        Spec.assertEqWith s "CR 731.2a and with the lifelink of the Cubwarden merged under it" [Map.member Keyword.Lifelink (PC.keywords (Revealed.characteristics r)) | GameEvent.Revealed r <- S.eventsOf under] [True]
        Spec.assertBool s (all (Maybe.isNothing . Game.lookupObject host) [over, under]) "setup: Murder destroyed the pile on both legs"
  -- CR 701.20a over a stored copy effect: the same face-up pile over the
  -- face-down Kirin, but Mirrorweave first makes it a copy of bob's Hill Giant
  -- until end of turn. The copy effect made the PERMANENT a Hill Giant, so the
  -- Kirin component Murder makes alice reveal is still shown as its card.
  Spec.it s "CR 701.20a a face-down component is revealed as its card, not the copy its pile was" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    kirin <- S.printingOf s registry "Misthoof Kirin"
    cubwarden <- S.printingOf s registry "Cubwarden"
    murder <- S.printingOf s registry "Murder"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    giant <- S.printingOf s registry "Hill Giant"
    case faceDownBoard plains kirin cubwarden of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (host, board, spellId) -> do
        let (giantId, withGiant) = S.addPermanent giant S.bob (merging MutateSide.Over host board spellId)
            (weaving, weaveId) = S.handOne mirrorweave (S.landsFor plains S.alice 4 withGiant)
            atGiant :: Prompt.Prompt r -> r
            atGiant p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just giantId) . Recipient.objectOf) . snd) sets
              _ -> S.identityAnswer p
            woven = S.runPure atGiant weaving (S.cast S.alice weaveId >> Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
            (armed, murderId) = S.handOne murder (S.landsFor swamp S.alice 3 woven)
            murdered = S.runPure (mutatingAt MutateSide.Over host) armed (S.cast S.alice murderId >> Monad.replicateM_ 3 (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)
        Spec.assertEqWith s "CR 701.20a the reveal shows the Kirin card, not the Hill Giant its pile was a copy of" (departureRevealNames murdered) [Set.singleton (CardName.MkCardName (Text.pack "Misthoof Kirin"))]
        Spec.assertEqWith s "setup: Mirrorweave made the pile a Hill Giant" (Projection.namesOf host woven) (Set.singleton (CardName.MkCardName (Text.pack "Hill Giant")))
        Spec.assertBool s (Maybe.isNothing (Game.lookupObject host murdered)) "setup: Murder destroyed the pile"
  -- CR 731.2a under a STAMPED copy effect: Cubwarden merges under the face-down
  -- Kirin, so the pile is face down, and Infinite Reflection's trigger makes it
  -- a copy of alice's Hill Giant with no stated ending (CR 708.10). Bob's
  -- Hauntwoods Shrieker, out of the trigger's reach, reveals the pile: the copy
  -- effect changed the permanent, never its merged cards.
  Spec.it s "CR 731.2a a face-down pile Infinite Reflection made a copy is revealed with its merge" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    kirin <- S.printingOf s registry "Misthoof Kirin"
    cubwarden <- S.printingOf s registry "Cubwarden"
    giant <- S.printingOf s registry "Hill Giant"
    reflection <- S.printingOf s registry "Infinite Reflection"
    shrieker <- S.printingOf s registry "Hauntwoods Shrieker"
    case faceDownBoard plains kirin cubwarden of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (host, board, spellId) -> do
        let (giantId, withGiant) = S.addPermanent giant S.alice (merging MutateSide.Under host board spellId)
            (shriekerId, withShrieker) = S.addPermanent shrieker S.bob (S.landsFor forest S.bob 2 withGiant)
            (reflecting, reflectionId) = S.handOne reflection (S.landsFor island S.alice 6 withShrieker)
            atGiant :: Prompt.Prompt r -> r
            atGiant p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just giantId) . Recipient.objectOf) . snd) sets
              _ -> S.identityAnswer p
            reflected = S.runPure atGiant reflecting (S.cast S.alice reflectionId >> Monad.replicateM_ 3 (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)
            armed =
              reflected
                { GameState.turn =
                    (GameState.turn reflected)
                      { TurnRegisters.phase = Phase.PrecombatMain,
                        TurnRegisters.activePlayer = S.alice,
                        TurnRegisters.priority = Just S.bob
                      }
                }
            -- The Shrieker's "may" declined, so the pile stays face down and
            -- the reveal is the whole of what it shows.
            atPile :: Prompt.Prompt r -> r
            atPile p = case p of
              Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just host) . Recipient.objectOf) . snd) sets
              Prompt.ChooseOptional {} -> OptionalDecision.Declines
              _ -> S.identityAnswer p
            shown gs = [Revealed.characteristics r | GameEvent.Revealed r <- S.eventsOf gs, Revealed.card r == host]
        case Activatable.abilitiesFor shriekerId armed of
          [ability] -> do
            let shrieked = S.runPure atPile armed (Activate.activateAbility S.bob shriekerId ability >> Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
            Spec.assertEqWith s "CR 731.2a the reveal carries the lifelink of the Cubwarden merged under the Kirin" (fmap (Map.member Keyword.Lifelink . PC.keywords) (shown shrieked)) [True]
            Spec.assertEqWith s "CR 701.20a and shows the topmost Kirin's name, not the Hill Giant the pile became" (fmap PC.names (shown shrieked)) [Set.singleton (CardName.MkCardName (Text.pack "Misthoof Kirin"))]
            Spec.assertEqWith s "setup: Infinite Reflection made the pile a copy of the Hill Giant" (fmap PC.names (Binding.copyOf . Object.bindings =<< Game.lookupObject host reflected)) (Just (Set.singleton (CardName.MkCardName (Text.pack "Hill Giant"))))
            Spec.assertBool s (maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject host shrieked)) "setup: the pile stayed face down"
          abilities -> Spec.assertFailure s ("expected the Shrieker's one activated ability, got " <> show (length abilities))
  -- CR 708.9's third sentence: the same pile's owner concedes a three-player
  -- game (CR 800.4a), so it leaves the game rather than moving to a zone, and
  -- the game goes on (the end of the game being the rule's fourth sentence).
  Spec.it s "CR 708.9 a face-down component is revealed as its owner leaves the game" $ do
    plains <- S.printingOf s registry "Plains"
    kirin <- S.printingOf s registry "Misthoof Kirin"
    cubwarden <- S.printingOf s registry "Cubwarden"
    case faceDownBoardOn S.threePlayers plains kirin cubwarden of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (host, board, spellId) -> do
        let conceded side = S.runPure S.identityAnswer (merging side host board spellId) (Departure.leaveGame Departure.Type.Conceded S.alice)
            over = conceded MutateSide.Over
        Spec.assertEqWith s "CR 708.9 alice revealed the face-down Kirin component as she left" (departureReveals over) [(S.alice, host, RevealCause.LeavingFaceDownComponent)]
        Spec.assertEqWith s "CR 708.9 merged under, the pile is face down and revealed whole" (departureReveals (conceded MutateSide.Under)) [(S.alice, host, RevealCause.LeavingFaceDown)]
        Spec.assertBool s (Maybe.isNothing (GameState.result over)) "setup: the game went on without her"
  -- CR 712.12a over CR 731.2e's mix: Cubwarden merges OVER a manifested
  -- Blightreaper Thallid, Maskwood Nexus makes the face-up pile a Human, and
  -- Moonmist transforms all Humans. The face-down Thallid is the pile's only
  -- double-faced component and can't transform, so nothing turns over. The
  -- paired leg merges over the same Thallid face up, and the pile transforms.
  Spec.it s "CR 712.12a Moonmist does not turn a face-down component of a face-up pile" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    cubwarden <- S.printingOf s registry "Cubwarden"
    nexus <- S.printingOf s registry "Maskwood Nexus"
    moonmist <- S.printingOf s registry "Moonmist"
    let (host, board, spellId) = mutateBoard plains thallid cubwarden
        manifested = board {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.Manifested}) host (GameState.objects board)}
        humanPile hostBoard = snd (S.addPermanent nexus S.alice (merging MutateSide.Over host hostBoard spellId))
        misted hostBoard =
          let (moonmistId, staged) = S.addHandCard moonmist S.alice (S.landsFor forest S.alice 2 (humanPile hostBoard))
           in S.runPure
                S.identityAnswer
                staged
                  { GameState.turn =
                      (GameState.turn staged)
                        { TurnRegisters.phase = Phase.PrecombatMain,
                          TurnRegisters.priority = Just S.alice
                        }
                  }
                (S.cast S.alice moonmistId *> Stack.resolveTop)
        transformed gs = [Transformed.object t | GameEvent.Transformed t <- S.eventsOf gs]
    Spec.assertEqWith s "CR 712.12a the pile over the face-down Thallid did not transform" (transformed (misted manifested)) []
    Spec.assertEqWith s "CR 701.27a over the face-up Thallid, it did" (transformed (misted board)) [host]
    Spec.assertEqWith s "setup: CR 731.2e the pile over the face-down Thallid is a face-up Cubwarden" (Projection.namesOf host (humanPile manifested)) (Set.singleton (CardName.MkCardName (Text.pack "Cubwarden")))
    Spec.assertBool s (Set.member Subtype.Human (PC.subtypes (Projection.project host (humanPile manifested)))) "setup: Maskwood Nexus made it a Human"
  -- CR 712.12a over the merge's turned reading: Synthetic Graftling merges OVER
  -- a manifested Blightreaper Thallid and transforms the pile by its own
  -- ability. The Graftling turns and the face-down Thallid does not, so the
  -- pile is a 4/4 Graftbeast with trample that keeps the Thallid FRONT face's
  -- activated ability (CR 702.140e), where Blightsower Thallid has none.
  Spec.it s "CR 712.12a a face-down component stays unturned when the pile transforms" $ do
    forest <- S.printingOf s registry "Forest"
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    graftling <- S.printingOf s registry "Synthetic Graftling"
    let base = S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent thallid S.alice base
        manifested = withHost {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.Manifested}) host (GameState.objects withHost)}
        (board, spellId) = S.handOne graftling manifested
        merged = merging MutateSide.Over host board spellId
        offered gs =
          length
            ( Activatable.abilitiesFor
                host
                (S.landsFor forest S.alice 4 gs)
                  { GameState.turn =
                      (GameState.turn (S.landsFor forest S.alice 4 gs))
                        { TurnRegisters.phase = Phase.PrecombatMain,
                          TurnRegisters.activePlayer = S.alice,
                          TurnRegisters.priority = Just S.alice
                        }
                  }
            )
        reading gs = (Projection.namesOf host gs, S.powerToughnessOf host gs, Projection.hasKeyword Keyword.Trample host gs)
    case transformedBy forest host merged of
      Nothing -> Spec.assertFailure s "expected the pile to offer the Graftling's activated ability"
      Just after -> do
        Spec.assertEqWith s "CR 712.12a the face-down Thallid's front-face ability is still the pile's one activated ability" (offered after) 1
        Spec.assertEqWith s "CR 731.2i the Graftling turned: a 4/4 Graftbeast with trample" (reading after) (Set.singleton graftbeastName, Just (4, 4), True)
        Spec.assertEqWith s "setup: merged, the pile offered the Graftling's ability and the Thallid's" (offered merged) 2
  -- CR 731.2i with TWO double-faced components: Synthetic Graftling transforms
  -- by its own ability, unmerged, and a second Graftling then mutates OVER it.
  -- The pile shows the top card's front and the transformed card's trample (CR
  -- 702.140e); transforming it turns EACH component over, so the top shows its
  -- back and the one under it its front, and a second transform turns both
  -- back. The discriminator is a Clone's mana value: CR 712.6d makes a copy of
  -- a back face 0, so a pile read as showing one face for both components gets
  -- the copy of its top's FRONT face wrong.
  Spec.it s "CR 731.2i each double-faced component of a merged permanent turns over on its own" $ do
    forest <- S.printingOf s registry "Forest"
    graftling <- S.printingOf s registry "Synthetic Graftling"
    clone <- S.printingOf s registry "Clone"
    let base = S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent graftling S.alice base
        (board, spellId) = S.handOne graftling withHost
        cloneOf gs =
          let (_, staged) = S.spellOnStack clone S.alice gs
              copied = S.runPure (copying host) staged (Stack.resolveTop >> Engine.settleForPriority)
           in fmap (\copy -> (Projection.namesOf copy copied, PC.manaValue (Projection.project copy copied))) (cloneOn copied)
        reading gs = (Projection.namesOf host gs, S.powerToughnessOf host gs, Projection.hasKeyword Keyword.Trample host gs)
    case transformedBy forest host board of
      Nothing -> Spec.assertFailure s "expected the unmerged Graftling to offer exactly one activated ability"
      Just alone -> do
        let merged = merging MutateSide.Over host alone spellId
        case transformedBy forest host merged of
          Nothing -> Spec.assertFailure s "expected the merged permanent to offer exactly one activated ability"
          Just turned -> case transformedBy forest host turned of
            Nothing -> Spec.assertFailure s "expected the turned permanent to offer the bottom Graftling's activated ability"
            Just back -> do
              Spec.assertEqWith
                s
                "CR 731.2i/707.2 a Clone of the merged permanent copies its top's FRONT face, mana value 2 (CR 202.3)"
                (cloneOf merged)
                (Just (Set.singleton graftlingName, Just 2))
              Spec.assertEqWith s "CR 731.2i transformed, the top shows its back: a 4/4 Graftbeast with trample" (reading turned) (Set.singleton graftbeastName, Just (4, 4), True)
              Spec.assertEqWith
                s
                "CR 712.6d a Clone of it then copies a back face, mana value 0"
                (cloneOf turned)
                (Just (Set.singleton graftbeastName, Just 0))
              Spec.assertEqWith s "CR 731.2i transformed again, each component turns back" (reading back) (Set.singleton graftlingName, Just (2, 2), True)
              Spec.assertEqWith
                s
                "CR 613.7g/731.2i a merged permanent is no double-faced permanent, so it keeps its timestamp"
                (fmap Object.timestamp (Game.lookupObject host turned))
                (fmap Object.timestamp (Game.lookupObject host merged))
              Spec.assertEqWith s "setup: merged, a 2/2 Graftling with the transformed card's trample" (reading merged) (Set.singleton graftlingName, Just (2, 2), True)
              Spec.assertEqWith s "setup: CR 701.27a the unmerged Graftling transformed into a 4/4 Graftbeast" (reading alone) (Set.singleton graftbeastName, Just (4, 4), True)
              Spec.assertEqWith s "setup: two Graftlings represent one permanent" (componentNames host merged) [graftlingName, graftlingName]
  -- CR 731.2i over a merge onto a FACE-DOWN permanent: a manifested Blightreaper
  -- Thallid, Synthetic Graftling mutated UNDER it, so the pile stays face down
  -- (CR 731.2e); turned face up (CR 731.2f), the Graftling's ability transforms
  -- it, and both components turn over. The merge's turned reading was taken
  -- while the Thallid was face down, so a reading that refused a face-down turn
  -- (CR 712.12a) would leave the pile a 2/2 Blightreaper.
  Spec.it s "CR 731.2i a merged permanent that was face down transforms once turned face up" $ do
    forest <- S.printingOf s registry "Forest"
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    graftling <- S.printingOf s registry "Synthetic Graftling"
    let base = S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent thallid S.alice base
        manifested = withHost {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.Manifested}) host (GameState.objects withHost)}
        (board, spellId) = S.handOne graftling manifested
        merged = merging MutateSide.Under host board spellId
        turnedUp = S.runPure S.identityAnswer merged (Event.performTurnFaceUp S.alice Nothing Nothing host)
        reading gs = (Projection.namesOf host gs, S.powerToughnessOf host gs, Projection.hasKeyword Keyword.Trample host gs)
    case transformedBy forest host turnedUp of
      Nothing -> Spec.assertFailure s "expected the pile to offer the Graftling's activated ability"
      Just after -> do
        Spec.assertEqWith s "CR 731.2i a 3/3 Blightsower Thallid with the turned Graftling's trample" (reading after) (Set.singleton blightsower, Just (3, 3), True)
        Spec.assertEqWith s "setup: turned face up, a 2/2 Blightreaper Thallid" (reading turnedUp) (Set.singleton (CardName.MkCardName (Text.pack "Blightreaper Thallid")), Just (2, 2), False)
        Spec.assertBool s (maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject host merged)) "setup: CR 731.2e the pile was face down after the merge"
  -- CR 731.2f: a manifested Blightreaper Thallid with two Cubwardens mutated
  -- UNDER it, one at a time, so the second merge lands on a face-down PILE.
  -- Turned face up, the pile turns its face-down Thallid component up with it,
  -- and the Thallid's own ability then transforms it into a 3/3 Blightsower.
  -- Left face down, the component could not turn (CR 712.12a), and a second
  -- merge reading the pile's components as still face down would stamp no
  -- turned Thallid.
  Spec.it s "CR 731.2f a face-down pile turned face up turns its components up with it" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let base = S.landsFor plains S.alice 8 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent thallid S.alice base
        manifested = withHost {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.Manifested}) host (GameState.objects withHost)}
        (withFirst, first) = S.handOne cubwarden manifested
        (board, second) = S.handOne cubwarden withFirst
        merged = merging MutateSide.Under host (merging MutateSide.Under host board first) second
        turnedUp = S.runPure S.identityAnswer merged (Event.performTurnFaceUp S.alice Nothing Nothing host)
        armed =
          (S.landsFor forest S.alice 4 turnedUp)
            { GameState.turn =
                (GameState.turn (S.landsFor forest S.alice 4 turnedUp))
                  { TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        reading gs = (Projection.namesOf host gs, S.powerToughnessOf host gs)
    case Activatable.abilitiesFor host armed of
      [ability] -> do
        let activated = S.runPure S.identityAnswer armed (Activate.activateAbility S.alice host ability)
            after = S.runPure S.identityAnswer activated (Monad.replicateM_ 4 (Stack.resolveTop >> Engine.settleForPriority))
        Spec.assertEqWith s "CR 731.2f/731.2i the Thallid turned up with the pile and then transformed: a 3/3 Blightsower Thallid" (reading after) (Set.singleton blightsower, Just (3, 3))
        Spec.assertEqWith s "setup: turned face up, a 2/2 Blightreaper Thallid" (reading turnedUp) (Set.singleton (CardName.MkCardName (Text.pack "Blightreaper Thallid")), Just (2, 2))
        Spec.assertEqWith s "setup: CR 731.2e three cards represent the face-down pile" (componentNames host merged, fmap Object.facing (Game.lookupObject host merged)) ([CardName.MkCardName (Text.pack "Blightreaper Thallid"), CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Cubwarden")], Just (Facing.faceDown FaceDownReason.Manifested))
      offered -> Spec.assertFailure s ("expected the turned-up pile to offer the Thallid's one activated ability, got " <> show (length offered))
  -- CR 202.3b over a merge a later copy effect replaced: Synthetic Graftling
  -- mutates over Unstable Shapeshifter, the pile transforms, and a Hill Giant
  -- entering makes it a copy of the Hill Giant through the trigger it carries
  -- from under (CR 702.140e). Its copiable values are the Hill Giant's now, not
  -- a back face, so a Clone of it has mana value 4.
  Spec.it s "CR 202.3b a Clone of a transformed pile that became a Hill Giant has mana value 4" $ do
    forest <- S.printingOf s registry "Forest"
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    graftling <- S.printingOf s registry "Synthetic Graftling"
    hillGiant <- S.printingOf s registry "Hill Giant"
    clone <- S.printingOf s registry "Clone"
    let base = S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent shapeshifter S.alice base
        (board, spellId) = S.handOne graftling withHost
        merged = merging MutateSide.Over host board spellId
        entering gs =
          let (_, staged) = S.spellOnStack hillGiant S.alice gs
           in S.runPure S.identityAnswer staged (Monad.replicateM_ 3 (Stack.resolveTop >> Engine.settleForPriority))
        cloneOf gs =
          let (_, staged) = S.spellOnStack clone S.alice gs
              copied = S.runPure (copying host) staged (Stack.resolveTop >> Engine.settleForPriority)
           in fmap (\copy -> PC.manaValue (Projection.project copy copied)) (cloneOn copied)
    case transformedBy forest host merged of
      Nothing -> Spec.assertFailure s "expected the pile to offer the Graftling's activated ability"
      Just turned -> do
        let copied = entering turned
        Spec.assertEqWith s "CR 202.3b a Clone of it has the Hill Giant's mana value 4" (cloneOf copied) (Just (Just 4))
        Spec.assertEqWith s "setup: the pile became a copy of the Hill Giant" (Projection.namesOf host copied) (Set.singleton (CardName.MkCardName (Text.pack "Hill Giant")))
        Spec.assertEqWith s "setup: CR 712.6d before that, a Clone of the transformed pile copied a back face, mana value 0" (cloneOf turned) (Just (Just 0))
  -- CR 731.2h beside CR 731.2i: Synthetic Graftling mutates OVER an unflipped
  -- Akki Lavarunner, the pile connects and Akki's printed trigger flips it, and
  -- the Graftling's ability then transforms it. The flip component must stay
  -- flipped while the double-faced one turns over, so the pile is a 4/4
  -- Graftbeast with trample beside Tok-Tok's protection from red -- not the
  -- flipped 2/2 Graftling it was before the transform.
  Spec.it s "CR 731.2h/731.2i a flipped merged permanent transforms and stays flipped" $ do
    forest <- S.printingOf s registry "Forest"
    akki <- S.printingOf s registry "Akki Lavarunner"
    graftling <- S.printingOf s registry "Synthetic Graftling"
    let base = S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent akki S.alice base
        (board, spellId) = S.handOne graftling withHost
        merged = merging MutateSide.Over host board spellId
        flipped = S.runCombat S.aggressiveAnswer (intoCombat merged)
        reading gs = (Projection.namesOf host gs, S.powerToughnessOf host gs, Projection.hasKeyword Keyword.Trample host gs, Projection.hasKeyword protectionFromRed host gs)
    case transformedBy forest host flipped of
      Nothing -> Spec.assertFailure s "expected the flipped merged permanent to offer exactly one activated ability"
      Just after -> do
        Spec.assertEqWith
          s
          "CR 731.2h/731.2i a 4/4 Graftbeast with trample and the flipped Tok-Tok's protection from red"
          (reading after)
          (Set.singleton graftbeastName, Just (4, 4), True, True)
        Spec.assertEqWith s "setup: CR 110.5 still flipped" (fmap Object.flipped (Game.lookupObject host after)) (Just True)
        Spec.assertEqWith s "setup: flipped, a 2/2 Graftling with Tok-Tok's protection from red" (reading flipped) (Set.singleton graftlingName, Just (2, 2), False, True)
        Spec.assertEqWith s "setup: merged, a 2/2 Graftling without it" (reading merged) (Set.singleton graftlingName, Just (2, 2), False, False)
        Spec.assertEqWith s "setup: the Graftling over Akki Lavarunner" (componentNames host merged) [graftlingName, CardName.MkCardName (Text.pack "Akki Lavarunner")]
  -- CR 731.2i under a stored copy: Mirrorweave makes the Falcon Abomination a
  -- Hill Giant until end of turn, Synthetic Graftling mutates over it, and the
  -- Graftling's ability transforms the pile that same turn. The merge applies
  -- over Mirrorweave's copy (CR 613.7), so the turned-over Graftling shows at
  -- once -- a 4/4 Graftbeast with trample -- and once cleanup ends the copy, the
  -- Falcon Abomination's flying joins it from under.
  Spec.it s "CR 731.2i/613.7 a merged permanent transforms while Mirrorweave's copy stands" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    falcon <- S.printingOf s registry "Falcon Abomination"
    hillGiant <- S.printingOf s registry "Hill Giant"
    graftling <- S.printingOf s registry "Synthetic Graftling"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    let base = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent falcon S.alice base
        (giant, withGiant) = S.addPermanent hillGiant S.alice withHost
        (staged, weaveId) = S.handOne mirrorweave withGiant
        woven = S.runPure (mutatingAt MutateSide.Over giant) staged (S.cast S.alice weaveId >> Stack.resolveTop >> Engine.settleForPriority)
        (board, spellId) = S.handOne graftling (S.landsFor forest S.alice 1 woven)
        merged = merging MutateSide.Over host board spellId
        step g = S.runPure S.identityAnswer g Engine.runStep
        cleanedUp gs =
          step
            ( step
                ( step
                    gs
                      { GameState.turn =
                          (GameState.turn gs)
                            { TurnRegisters.remaining = Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
                            }
                      }
                )
            )
        reading gs = (Projection.namesOf host gs, S.powerToughnessOf host gs, Projection.hasKeyword Keyword.Trample host gs, Projection.hasKeyword Keyword.Flying host gs)
    case transformedBy forest host merged of
      Nothing -> Spec.assertFailure s "expected the merged permanent to offer exactly one activated ability"
      Just turned -> do
        Spec.assertEqWith s "CR 731.2i/613.7 transformed under Mirrorweave's copy, it is a 4/4 Graftbeast with trample" (reading turned) (Set.singleton graftbeastName, Just (4, 4), True, False)
        Spec.assertEqWith s "CR 611.2a after cleanup it is still the Graftbeast, with the Falcon Abomination's flying from under" (reading (cleanedUp turned)) (Set.singleton graftbeastName, Just (4, 4), True, True)
        Spec.assertEqWith s "setup: merged under Mirrorweave's copy, a 2/2 Graftling with nothing from under" (reading merged) (Set.singleton graftlingName, Just (2, 2), False, False)
        Spec.assertEqWith s "setup: under Mirrorweave the Falcon Abomination was the Hill Giant" (Projection.namesOf host board) (Set.singleton (CardName.MkCardName (Text.pack "Hill Giant")))
  -- CR 702.140e over a Room on top: Opalescence makes the Room with its red
  -- door open a 2/2 non-Human creature, and Cubwarden mutates under it. The
  -- merged permanent shows the Room (CR 731.2a), whose characteristics are
  -- rebuilt from its halves against its own doors (CR 709.5), and the rebuild
  -- still carries the lifelink only the component under it prints.
  Spec.it s "CR 702.140e mutating under an animated Room keeps the abilities from under it" $ do
    plains <- S.printingOf s registry "Plains"
    opalescence <- S.printingOf s registry "Opalescence"
    room <- S.printingOf s registry "Roaring Furnace"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let base = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
        (_, withOpalescence) = S.addPermanent opalescence S.alice base
        (host, withRoom) = S.addPermanent room S.alice withOpalescence
        opened = withRoom {GameState.objects = Map.adjust (\o -> o {Object.unlockedHalves = Set.singleton RoomHalf.LeftHalf}) host (GameState.objects withRoom)}
        (board, spellId) = S.handOne cubwarden opened
        after = merging MutateSide.Under host board spellId
    Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink host after) "the merged Room has Cubwarden's lifelink"
  -- CR 731.2d with a COPY topmost: Double Major copies alice's mutating Cubwarden
  -- and the copy merges over her Falcon Abomination. The copy becomes a token as
  -- it resolves (CR 111.13, 707.10f), so the merged permanent is a token.
  --
  -- THREE resolutions -- Double Major, the copy, rule 702.140d's Cat trigger --
  -- with the original Cubwarden left waiting, so one CR 702.140c side prompt is
  -- asked and the copy is the topmost component. Draining the rest puts the
  -- original over it, a card topmost, which is the control.
  --
  -- Ashaya, Soul of the Wild reads token-ness on the battlefield and Midnight
  -- Reaper ("whenever a nontoken creature you control dies") reads it as the
  -- permanent dies, each against the same control.
  Spec.it s "CR 731.2d/111.13 a merged permanent whose topmost component is a spell copy is a token" $ do
    plains <- S.printingOf s registry "Plains"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    doubleMajor <- S.printingOf s registry "Double Major"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    reaper <- S.printingOf s registry "Midnight Reaper"
    let base = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 (S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)))
        (host, withHost) = S.addPermanent falcon S.alice base
        (_, withAshaya) = S.addPermanent ashaya S.alice withHost
        (_, withReaper) = S.addPermanent reaper S.alice withAshaya
        (withSpell, spellId) = S.handOne cubwarden withReaper
        (board, copyId) = S.handOne doubleMajor withSpell
        cast = S.runPure (doublingAt host) board (S.cast S.alice spellId >> S.cast S.alice copyId)
        passes n = Monad.replicateM_ n (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority
        after = S.runPure (doublingAt host) cast (passes 3)
        -- The original Cubwarden resolves over the copy, and its Cat trigger.
        covered = S.runPure (doublingAt host) after (passes 4)
        -- The merged permanent dies, and the stack drains: Midnight Reaper's
        -- trigger if it fired, then whatever else waited.
        dies gs = S.runPure (doublingAt host) gs (Event.destroy Settle.none Regenerability.Regenerable [host] >> passes 6)
        isForest oid gs = Set.member Subtype.Forest (Projection.subtypesOf oid gs)
    Spec.assertBool s (not (isForest host after)) "CR 731.2d/111.13 the copy on top is a token, so Ashaya passes the merged permanent by"
    Spec.assertEqWith s "CR 731.2d/111.13 so Midnight Reaper does not trigger as it dies" (S.lifeOf S.alice (dies after)) (Just 20)
    Spec.assertBool s (isForest host covered) "CR 731.2d with the original Cubwarden over it a card is topmost, so Ashaya makes it a Forest"
    Spec.assertEqWith s "and Midnight Reaper triggers as it dies" (S.lifeOf S.alice (dies covered)) (Just 19)
    -- The proxies, after the behaviours: the copy really is topmost, over the
    -- creature it merged with, and the original still waits on the stack.
    Spec.assertEqWith
      s
      "CR 731.2a/111.13 the copy is the merged permanent's topmost component, a token"
      (componentKinds host after)
      [ (Text.pack "OfToken", CardName.MkCardName (Text.pack "Cubwarden")),
        (Text.pack "OfCard", CardName.MkCardName (Text.pack "Falcon Abomination"))
      ]
    Spec.assertEqWith s "and the original Cubwarden still waits on the stack" (length (TurnRegisters.stack (GameState.turn after))) 1
    Spec.assertEqWith s "setup: the original went over the copy" (fmap fst (componentKinds host covered)) (fmap Text.pack ["OfCard", "OfToken", "OfCard"])
    Spec.assertBool s (not (isForest (catId after) after)) "setup: Ashaya passes a Cat token by"
  -- CR 613.7 / 707.3 over CR 731.2h: Mirrorweave makes the flipped merged
  -- Tok-Tok a copy of bob's Craw Wurm. Its copy effect is later than the merge's,
  -- so the merged permanent is a 6/4 Craw Wurm, and its flipped status has no
  -- effect on a copy of a card with no flip half (CR 110.5c).
  Spec.it s "CR 707.3 Mirrorweave makes a flipped merged permanent a copy of a creature with no flip half" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    akki <- S.printingOf s registry "Akki Lavarunner"
    cubwarden <- S.printingOf s registry "Cubwarden"
    wurm <- S.printingOf s registry "Craw Wurm"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    let (host, board, spellId) = mutateBoard plains akki cubwarden
        flipped = S.runCombat S.aggressiveAnswer (intoCombat (merging MutateSide.Under host board spellId))
        (wurmId, withWurm) = S.addPermanent wurm S.bob (S.landsFor island S.alice 4 flipped)
        (staged, weaveId) = S.handOne mirrorweave withWurm
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just wurmId) . Recipient.objectOf) . snd) sets
          _ -> S.identityAnswer p
        woven = S.runPure answer staged (S.cast S.alice weaveId >> Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith
      s
      "CR 613.7 the later copy effect wins: the flipped merged permanent is a 6/4 Craw Wurm"
      (Projection.namesOf host woven, S.powerToughnessOf host woven)
      (Set.singleton (CardName.MkCardName (Text.pack "Craw Wurm")), Just (6, 4))
    Spec.assertBool s (not (Game.flipsOver host woven)) "CR 707.3 and a copy of a card with no flip half has nothing to flip to"
    Spec.assertEqWith s "setup: CR 110.5 the status is kept" (fmap Object.flipped (Game.lookupObject host woven)) (Just True)
    Spec.assertEqWith s "setup: before Mirrorweave it was a 2/2 Tok-Tok" (Projection.namesOf host flipped, S.powerToughnessOf host flipped) (Set.singleton (CardName.MkCardName (Text.pack "Tok-Tok, Volcano Born")), Just (2, 2))
  -- CR 731.2h under a stored copy: Mirrorweave makes Craw Wurm a copy of Akki
  -- Lavarunner, Cubwarden mutates under it, and the copied Akki's trigger flips
  -- the merged permanent. The merge applies over Mirrorweave's row (CR 613.7), so
  -- the flipped reading is Tok-Tok carrying Cubwarden's lifelink (CR 702.140e),
  -- not a bare Tok-Tok and not the Craw Wurm under the row. The Akki is bob's and
  -- tapped, so it neither blocks nor flips into a second legendary Tok-Tok.
  Spec.it s "CR 731.2h a merge over Mirrorweave's copy of a flip card flips with the abilities from under" $ do
    plains <- S.printingOf s registry "Plains"
    akki <- S.printingOf s registry "Akki Lavarunner"
    wurm <- S.printingOf s registry "Craw Wurm"
    cubwarden <- S.printingOf s registry "Cubwarden"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    let base = S.landsFor plains S.alice 8 (Setup.emptyGame S.bothPlayers)
        (akkiId, withAkki) = S.addPermanent akki S.bob base
        (host, withHost) = S.addPermanent wurm S.alice (S.tapObject akkiId withAkki)
        (staged, weaveId) = S.handOne mirrorweave withHost
        woven = S.runPure (mutatingAt MutateSide.Over akkiId) staged (S.cast S.alice weaveId >> Stack.resolveTop >> Engine.settleForPriority)
        (board, spellId) = S.handOne cubwarden woven
        merged = merging MutateSide.Under host board spellId
        after = S.runCombat S.aggressiveAnswer (intoCombat merged)
    Spec.assertEqWith
      s
      "CR 731.2h the flipped merged permanent is a 2/2 Tok-Tok, Volcano Born, without Akki's haste"
      (Projection.namesOf host after, S.powerToughnessOf host after, Projection.hasKeyword Keyword.Haste host after)
      (Set.singleton (CardName.MkCardName (Text.pack "Tok-Tok, Volcano Born")), Just (2, 2), False)
    Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink host after) "CR 702.140e with Cubwarden's lifelink from under"
    Spec.assertEqWith s "setup: CR 110.5 the status itself is set" (fmap Object.flipped (Game.lookupObject host after)) (Just True)
    Spec.assertEqWith
      s
      "setup: before the flip it was Mirrorweave's Akki Lavarunner with Cubwarden's lifelink"
      (Projection.namesOf host merged, Projection.hasKeyword Keyword.Lifelink host merged)
      (Set.singleton (CardName.MkCardName (Text.pack "Akki Lavarunner")), True)
  -- CR 903.9c's split names "the card that represents it and is a commander" --
  -- and CR 111.6's token is no card, so a merged commander's token component is
  -- put into the appropriate zone with every other non-commander component,
  -- whatever printing it was interned under.
  --
  -- HAND-BUILT, and the only case in this file that is: the component list is
  -- given a card and a token under ONE printing, which
  -- is what makes the two readings of the split differ, and no printing in
  -- data/cards/ mints a token copy of a card for a mutate spell to merge with.
  -- An audit fold-in from #3390 rather than a rule this pool can reach.
  Spec.it s "CR 903.9c/111.6 a merged commander's token component is not split off to the command zone" $ do
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (oid, base) = S.addPermanent cubwarden S.alice (Setup.emptyGame S.bothPlayers)
    case Game.lookupObject oid base >>= (printingBehind . Object.source) of
      Nothing -> Spec.assertFailure s "the fixture did not put a card onto the battlefield"
      Just pid -> do
        let board =
              (asMergeOfCardAndToken oid pid base)
                { GameState.players = Map.adjust (\p -> p {Player.commander = Set.fromList (Maybe.maybeToList (Game.lookupObject oid base >>= Object.identity))}) S.alice (GameState.players (asMergeOfCardAndToken oid pid base))
                }
            after = snd (S.runPureWith returningCommander board (Event.changeZoneReturning Settle.none oid Zone.Hand))
        Spec.assertEqWith
          s
          "CR 903.9c one card goes to the command zone, and the token component interned under the same printing does not follow it"
          (Maybe.mapMaybe (\c -> fmap Object.source (Game.lookupObject c after)) (Set.toList (GameState.command after)))
          [Source.OfCard pid]
        -- The fixture facts, after it: the permanent really was a merge of a
        -- card and a token under one printing, and CR 903.9b's offer
        -- really was accepted.
        Spec.assertEqWith s "setup: the components were a card and a token under one printing" (fmap (Foldable.toList . Game.componentsOf . Object.source) (Game.lookupObject oid board)) (Just [MergeComponent.OfCard (ComponentCard.MkComponentCard pid (Game.lookupObject oid base >>= Object.identity) Nothing), MergeComponent.OfToken pid])
        Spec.assertEqWith s "setup: the merged permanent left the battlefield" (Game.lookupObject oid after) Nothing
  -- CR 731.3e's second sentence: Cubwarden mutated UNDER its own Cat token is a
  -- token (CR 731.2d), so bob's Leyline of the Void reaches only its card. The
  -- permanent dies through its token component (CR 700.4) while Cubwarden is
  -- exiled; merged OVER the Cat, the permanent is no token and the first
  -- sentence exiles both, which is the control.
  Spec.it s "CR 731.3e a token-topped merged permanent dies while Leyline of the Void exiles its card" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    leyline <- S.printingOf s registry "Leyline of the Void"
    let base = S.landsFor plains S.alice 8 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent falcon S.alice base
        (firstBoard, firstSpell) = S.handOne cubwarden withHost
        catBoard = merging MutateSide.Over host firstBoard firstSpell
        cat = catId catBoard
        (secondBoard, secondSpell) = S.handOne cubwarden catBoard
        mergedAt side = merging side cat secondBoard secondSpell
        dead side =
          let (_, withLeyline) = S.addPermanent leyline S.bob (mergedAt side)
           in S.runPure S.identityAnswer withLeyline (Event.destroy Settle.none Regenerability.Regenerable [cat] >> Engine.settleForPriority)
        exiledNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Set.toList (GameState.exile gs))
        cubwardenName = CardName.MkCardName (Text.pack "Cubwarden")
        died gs = [() | GameEvent.Moved m <- S.eventsOf gs, Moved.change m == ZoneChange.MkZoneChange cat (ZoneChange.object (Moved.change m)) Zone.Battlefield Zone.Graveyard]
        under = dead MutateSide.Under
        over = dead MutateSide.Over
    Spec.assertBool s (Game.isToken cat (mergedAt MutateSide.Under)) "setup: CR 731.2d merged under the Cat, the permanent is a token"
    Spec.assertBool s (not (Game.isToken cat (mergedAt MutateSide.Over))) "setup: CR 731.2d merged over it, the permanent is no token"
    Spec.assertEqWith s "CR 731.3e the card component under the token is exiled" (exiledNames under) [cubwardenName]
    Spec.assertBool s (notElem cubwardenName (graveyardNames under)) "CR 731.3e and not put into the graveyard"
    Spec.assertEqWith s "CR 700.4 the permanent itself dies, its token component put into the graveyard" (length (died under)) 1
    Spec.assertEqWith s "CR 731.3e the token is put into the graveyard and announced there, the card into exile" [ZoneChange.to zc | GameEvent.CardArrived zc <- S.eventsOf under, ZoneChange.departed zc == cat] [Zone.Exile]
    Spec.assertEqWith s "CR 731.3e merged over the Cat, both components follow Leyline of the Void" (exiledNames over) [cubwardenName]
    Spec.assertEqWith s "CR 731.3e and the permanent does not die" (length (died over)) 0
    Spec.assertEqWith s "CR 731.3e nor does anything reach the graveyard" (graveyardNames over) []
  -- CR 731.3b over CR 731.3e's split: two Cubwardens under a Cat token are both
  -- exiled by bob's Leyline of the Void, so bob, its controller, orders their
  -- timestamps. Asking anyone else, or nobody, leaves the reversed run equal to
  -- the canonical one.
  Spec.it s "CR 731.3b/731.3e the controller of the row that exiled a token-topped permanent's cards orders them" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    leyline <- S.printingOf s registry "Leyline of the Void"
    let base = S.landsFor plains S.alice 12 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent falcon S.alice base
        (firstBoard, firstSpell) = S.handOne cubwarden withHost
        catBoard = merging MutateSide.Over host firstBoard firstSpell
        cat = catId catBoard
        (secondBoard, secondSpell) = S.handOne cubwarden catBoard
        once = merging MutateSide.Under cat secondBoard secondSpell
        (thirdBoard, thirdSpell) = S.handOne cubwarden once
        twice = merging MutateSide.Under cat thirdBoard thirdSpell
        (_, withLeyline) = S.addPermanent leyline S.bob twice
        reversingFor :: Maybe PlayerId.PlayerId -> Prompt.Prompt r -> r
        reversingFor who p = case p of
          Prompt.OrderTimestamps _ pid oids | Just pid == who -> reverse (zipWith const [0 ..] oids)
          _ -> S.identityAnswer p
        dead who = S.runPure (reversingFor who) withLeyline (Event.destroy Settle.none Regenerability.Regenerable [cat] >> Engine.settleForPriority)
        stamps gs = fmap Object.timestamp (Maybe.mapMaybe (`Game.lookupObject` gs) (Set.toAscList (GameState.exile gs)))
        canonical = dead Nothing
        reversed = dead (Just S.bob)
    Spec.assertEqWith s "setup: both Cubwardens sit under the Cat" (length (filter (== CardName.MkCardName (Text.pack "Cubwarden")) (componentNames cat withLeyline))) 2
    Spec.assertEqWith s "setup: CR 731.3e both are exiled" (fmap length [stamps canonical, stamps reversed]) [2, 2]
    Spec.assertEqWith s "CR 731.3b bob's order is the one their timestamps take" (stamps reversed) (reverse (stamps canonical))
  -- CR 731.3e with CR 616.1: bob's Leyline of the Void and his Wheel of Sun and
  -- Moon enchanting alice both reach only the card under the Cat token, and
  -- alice picks which applies. The one picked settles where Cubwarden goes, and
  -- the other then has no card left to move: Wheel's reveal happens only when
  -- Wheel is the one picked, which sends Cubwarden to the bottom of her library.
  Spec.it s "CR 731.3e/616.1 one row reaching only the cards of a token-topped permanent leaves the other nothing to move" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    leyline <- S.printingOf s registry "Leyline of the Void"
    wheel <- S.printingOf s registry "Wheel of Sun and Moon"
    let base = S.landsFor plains S.alice 8 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent falcon S.alice base
        (firstBoard, firstSpell) = S.handOne cubwarden withHost
        catBoard = merging MutateSide.Over host firstBoard firstSpell
        cat = catId catBoard
        (secondBoard, secondSpell) = S.handOne cubwarden catBoard
        merged = merging MutateSide.Under cat secondBoard secondSpell
        (leylineId, withLeyline) = S.addPermanent leyline S.bob merged
        (wheelId, withWheel) = S.addPermanent wheel S.bob withLeyline
        (_, stocked) = S.addLibraryCard plains S.alice withWheel
        enchanting = S.attachTo wheelId (Recipient.ToPlayer S.alice) stocked
        plainsName = CardName.MkCardName (Text.pack "Plains")
        picking :: ObjectId.ObjectId -> Prompt.Prompt r -> r
        picking chosen p = case p of
          Prompt.ChooseReplacement _ _ entries -> maybe 0 Int.toNaturalSaturating (List.findIndex ((== chosen) . ReplacementEntry.source) entries)
          _ -> S.identityAnswer p
        dead chosen = S.runPure (picking chosen) enchanting (Event.destroy Settle.none Regenerability.Regenerable [cat] >> Engine.settleForPriority)
        exiledNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Set.toList (GameState.exile gs))
        cubwardenName = CardName.MkCardName (Text.pack "Cubwarden")
        reveals gs = length [() | GameEvent.Revealed r <- S.eventsOf gs, Revealed.cause r == RevealCause.Ordinary]
        byLeyline = dead leylineId
        byWheel = dead wheelId
    Spec.assertEqWith s "CR 731.3e Leyline picked, Cubwarden is exiled" (exiledNames byLeyline) [cubwardenName]
    Spec.assertEqWith s "CR 731.3e and Wheel, with no card left to move, reveals nothing" (reveals byLeyline) 0
    Spec.assertEqWith s "CR 731.3e Wheel picked, Cubwarden goes to the bottom of alice's library" (libraryNames byWheel) [plainsName, cubwardenName]
    Spec.assertEqWith s "CR 731.3e revealed once, by Wheel" (reveals byWheel) 1
    Spec.assertEqWith s "CR 731.3e and Leyline exiles nothing" (exiledNames byWheel) []
  -- CR 731.3c read by rule 702.55a's "exile it": Cubwarden mutated over Blind
  -- Hunter has haunt (CR 702.140e), and when it dies its `became` names BOTH
  -- cards, so both are exiled haunting the one target (CR 702.55b's "cards").
  -- Blind Hunter, {2}{W}{B} Creature -- Bat 2/2, Oracle verified 2026-10-02:
  -- "Flying / Haunt / When this creature enters or the creature it haunts dies,
  -- target player loses 2 life and you gain 2 life." Two identical Pikers, so
  -- the haunt target is a real choice and the drain reads the link, not a kill.
  Spec.it s "CR 731.3c/702.55b a mutated creature with haunt exiles both cards haunting its target" $ do
    plains <- S.printingOf s registry "Plains"
    hunter <- S.printingOf s registry "Blind Hunter"
    piker <- S.printingOf s registry "Goblin Piker"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let base = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent hunter S.alice base
        (victim, withVictim) = S.addPermanent piker S.bob withHost
        (_, withBystander) = S.addPermanent piker S.bob withVictim
        (board, spellId) = S.handOne cubwarden withBystander
        merged = merging MutateSide.Over host board spellId
        -- Every target pinned by FILTERING the offered set: the haunt slot down
        -- to the victim, the drain's "target player" down to bob.
        aimed :: Prompt.Prompt r -> r
        aimed p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (\r -> Recipient.objectOf r == Just victim || r == Recipient.ToPlayer S.bob) . snd) sets
          _ -> S.identityAnswer p
        dies oid gs = S.runPure aimed gs (Event.destroy Settle.none Regenerability.Regenerable [oid] >> Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
        exiled = dies host merged
        drained = dies victim exiled
        exileNames gs = List.sort (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Set.toList (GameState.exile gs)))
    Spec.assertEqWith s "CR 731.3c both cards are exiled" (exileNames exiled) (List.sort [CardName.MkCardName (Text.pack "Blind Hunter"), CardName.MkCardName (Text.pack "Cubwarden")])
    Spec.assertEqWith s "CR 702.55b each haunts the targeted Piker" (GameState.haunting exiled) (Map.fromSet (const victim) (GameState.exile exiled))
    Spec.assertEqWith s "CR 702.55c the haunted Piker's death drains bob for alice" (S.lifeOf S.alice drained, S.lifeOf S.bob drained) (Just 22, Just 18)
    Spec.assertEqWith s "setup: the merged permanent held two components" (fmap (Seq.length . Game.componentsOf . Object.source) (Game.lookupObject host merged)) (Just 2)
  -- CR 702.140f on Generator Servant ({1}{R} 2/1 Creature -- Elemental, "{T},
  -- Sacrifice this creature: Add {C}{C}. If any of that mana is spent on a
  -- creature spell, it gains haste until end of turn."): its haste names the
  -- mutating Cubwarden spell, and so the permanent it merges with, a Falcon
  -- Abomination that arrived this turn. The control pays the same {2}{W}{W}
  -- from Plains alone.
  Spec.it s "CR 702.140f haste Generator Servant's mana gave a mutating spell lets the merged permanent attack" $ do
    plains <- S.printingOf s registry "Plains"
    cubwarden <- S.printingOf s registry "Cubwarden"
    falcon <- S.printingOf s registry "Falcon Abomination"
    servant <- S.printingOf s registry "Generator Servant"
    let attacking side withServant =
          let (h, b, sp) = mutateBoard plains falcon cubwarden
              (servantId, b1) = S.addPermanent servant S.alice b
              board = if withServant then b1 else b
              sick = board {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) h (GameState.objects board)}
              servantFirst :: Prompt.Prompt r -> r
              servantFirst p = case p of
                Prompt.ChooseManaSource _ _ _ candidates | elem servantId candidates -> Just servantId
                _ -> mutatingAt side h p
              merged = mergingWith servantFirst sick sp
           in (h, merged, S.runPure S.aggressiveAnswer (intoCombat merged) (Combat.declareAttackers S.manaPerformer S.alice))
    Monad.forM_ [MutateSide.Over, MutateSide.Under] $ \side -> do
      let (host, merged, after) = attacking side True
          (cHost, cMerged, control) = attacking side False
      Spec.assertEqWith s ("CR 702.140f/702.10 the merged permanent attacks the turn its host arrived, " <> show side) (S.attackerDeclarationsOf after) [host]
      Spec.assertEqWith s ("CR 302.6 paid from Plains alone, it may not, " <> show side) (S.attackerDeclarationsOf control) []
      -- The proxies, after the behaviours.
      Spec.assertEqWith s "setup: the Servant was sacrificed for its mana" (S.countOnBattlefieldByName (S.printingName servant) S.alice merged) 0
      Spec.assertEqWith s "setup: the spell merged with the host" (length (componentNames host merged)) 2
      Spec.assertEqWith s "setup: and the control's did too" (length (componentNames cHost cMerged)) 2
  -- CR 702.140e / 604.3: Tarmogoyf's power-and-toughness ability is one of the
  -- abilities a component brings, so it survives a Cubwarden merged over it.
  -- The graveyards hold a land and an instant, so the ability says 2/3 where
  -- Cubwarden's box says 3/5, and both orders and a Clone of the pile read the
  -- count. Tarmogoyf cannot mutate, so CR 702.140c's two orders are the whole
  -- of what one Tarmogoyf can be merged into.
  Spec.it s "CR 702.140e a Cubwarden merged over or under Tarmogoyf keeps its power and toughness ability" $ do
    plains <- S.printingOf s registry "Plains"
    bolt <- S.printingOf s registry "Lightning Bolt"
    goyf <- S.printingOf s registry "Tarmogoyf"
    cubwarden <- S.printingOf s registry "Cubwarden"
    clone <- S.printingOf s registry "Clone"
    let mutated side =
          let (h, b, sp) = mutateBoard plains goyf cubwarden
              stocked = snd (S.addGraveyardCard bolt S.bob (snd (S.addGraveyardCard plains S.bob b)))
           in (h, merging side h stocked sp)
        (over, overBoard) = mutated MutateSide.Over
        (under, underBoard) = mutated MutateSide.Under
        (_, staged) = S.spellOnStack clone S.alice overBoard
        copied = S.runPure (copying over) staged (Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith s "CR 613.4a merged over Tarmogoyf, the pile is 2/3 and not Cubwarden's 3/5" (S.powerToughnessOf over overBoard) (Just (2, 3))
    Spec.assertEqWith s "CR 731.2a and is otherwise the topmost Cubwarden" (Projection.namesOf over overBoard, Projection.hasKeyword Keyword.Lifelink over overBoard) (Set.singleton (CardName.MkCardName (Text.pack "Cubwarden")), True)
    Spec.assertEqWith s "CR 613.4a merged under Tarmogoyf, the pile is 2/3 too" (S.powerToughnessOf under underBoard) (Just (2, 3))
    case cloneOn copied of
      Nothing -> Spec.assertFailure s "the Clone should have entered as a copy of the merged permanent"
      Just copy ->
        Spec.assertEqWith s "CR 707.2a a Clone of the pile copies the ability, so it is a 2/3 Cubwarden" (S.powerToughnessOf copy copied, Projection.namesOf copy copied) (Just (2, 3), Set.singleton (CardName.MkCardName (Text.pack "Cubwarden")))
    Spec.assertEqWith s "the two cards represent one permanent" (componentNames over overBoard) [CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Tarmogoyf")]
  -- CR 604.3a / 613.4a: the grafted ability stays characteristic-defining, so
  -- it applies in layer 7a, BEFORE Harmonious Archon's "Non-Archon creatures
  -- have base power and toughness 3/3" in layer 7b, whatever the timestamps.
  -- docs/decisions.md, "Whether an ability a merge grafts is still
  -- characteristic-defining", records the reading. A regression fence: parun has
  -- no layer-7b road for a CDA, so no mutation reaches the other reading.
  Spec.it s "CR 604.3a/613.4a Harmonious Archon's 3/3 outranks the Tarmogoyf ability merged under a Cubwarden" $ do
    plains <- S.printingOf s registry "Plains"
    bolt <- S.printingOf s registry "Lightning Bolt"
    goyf <- S.printingOf s registry "Tarmogoyf"
    cubwarden <- S.printingOf s registry "Cubwarden"
    archon <- S.printingOf s registry "Harmonious Archon"
    let (host, board, spellId) = mutateBoard plains goyf cubwarden
        stocked = snd (S.addGraveyardCard bolt S.bob (snd (S.addGraveyardCard plains S.bob (snd (S.addPermanent archon S.bob board)))))
        merged = merging MutateSide.Over host stocked spellId
    Spec.assertEqWith s "CR 613.4b the Archon's base 3/3 applies after the grafted ability's 2/3" (S.powerToughnessOf host merged) (Just (3, 3))

-- The merged board moved to CR 508.1's declaration, on S.combatBoardOf's terms:
-- alice active, bob the defending player (CR 506.2), and the beginning of combat
-- step already past. The merge itself has to happen in a main phase, so the two
-- cannot come from one fixture.
intoCombat :: GameState.GameState -> GameState.GameState
intoCombat gs =
  gs
    { GameState.turn =
        (GameState.turn gs)
          { TurnRegisters.activePlayer = S.alice,
            TurnRegisters.phase = Phase.Combat CombatStep.DeclareAttackers,
            TurnRegisters.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
            TurnRegisters.remaining = S.phasesAfter (Phase.Combat CombatStep.DeclareAttackers)
          }
    }

-- Synthetic Graftling's "{2}{G}: Transform this permanent. Activate only as a
-- sorcery." activated on `host` in alice's main phase, paid by three fresh
-- Forests, and the stack drained. Nothing unless `host` offers exactly one
-- activated ability at that {2}{G}.
transformedBy :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Maybe GameState.GameState
transformedBy forest host gs =
  let armed =
        (S.landsFor forest S.alice 3 gs)
          { GameState.turn =
              (GameState.turn (S.landsFor forest S.alice 3 gs))
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.priority = Just S.alice
                }
          }
      graftTransform = ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Green)]
   in case filter ((== Just graftTransform) . Cost.Type.mana . ActivatedAbility.cost) (Activatable.abilitiesFor host armed) of
        [ability] ->
          let activated = S.runPure S.identityAnswer armed (Activate.activateAbility S.alice host ability)
           in Just (S.runPure S.identityAnswer activated (Monad.replicateM_ 4 (Stack.resolveTop >> Engine.settleForPriority)))
        _ -> Nothing

-- Synthetic Graftling's two faces.
graftlingName, graftbeastName :: CardName.CardName
graftlingName = CardName.MkCardName (Text.pack "Synthetic Graftling")
graftbeastName = CardName.MkCardName (Text.pack "Synthetic Graftbeast")

-- Blightreaper Thallid's back face.
blightsower :: CardName.CardName
blightsower = CardName.MkCardName (Text.pack "Blightsower Thallid")

-- CR 614.12a's as-enters copy choice answered with `victim`, PINNED to that id
-- rather than searched for, so a mutation cannot be repaired by an answerer that
-- finds another eligible creature.
copying :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copying victim p = case p of
  Prompt.ChooseCopyTarget _ _ _ offered -> List.find (== victim) offered
  _ -> S.identityAnswer p

-- The Clone permanent, found by the printing behind it rather than by the name
-- it now shows: a copy of Falcon Abomination answers Game.cardOf with the copied
-- card, so a name search would find the original instead.
cloneOn :: GameState.GameState -> Maybe ObjectId.ObjectId
cloneOn gs =
  List.find
    (\oid -> fmap (S.nameOf . Printing.card) (Game.printingOfObject oid gs) == Just (CardName.MkCardName (Text.pack "Clone")))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- The board every case above starts from: four Plains, one Falcon Abomination
-- settled on the battlefield, and Cubwarden in alice's hand. Four white mana is
-- exactly Cubwarden's mutate cost and its printed cost is {3}{W}, so BOTH
-- candidates are payable and only the announcement separates them.
mutateBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
mutateBoard plains falcon cubwarden =
  let base = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
      (host, withHost) = S.addPermanent falcon S.alice base
      (board, spellId) = S.handOne cubwarden withHost
   in (host, board, spellId)

-- mutateBoard's shape with a FACE-DOWN creature as the host, which is what CR
-- 731.2e's face-up-and-face-down mix needs. Misthoof Kirin -- {2}{W} 2/1
-- Creature -- Kirin, flying, vigilance, megamorph {1}{W} -- is cast face down for
-- CR 702.37a's {3} and resolves, and Cubwarden waits in hand. Nothing about the
-- host is written by hand: the permanent is face down because a morph cast put
-- it there, so CR 702.37e's special action can turn it back over.
--
-- TEN Plains, so all three actions are paid in one currency and none of them can
-- fail for want of mana: {3} for the morph cast, CR 702.140a's {2}{W}{W} for the
-- mutate, and CR 702.37b's megamorph {1}{W} for the turning over. Nothing here
-- reads the tapped count, so the slack costs no assertion.
--
-- Nothing when the morph cast did not land, which every caller reports rather
-- than asserting against a board it did not get.
faceDownBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe (ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
faceDownBoard = faceDownBoardOn S.bothPlayers

-- `faceDownBoard` at these seats.
faceDownBoardOn :: NonEmpty.NonEmpty PlayerId.PlayerId -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe (ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
faceDownBoardOn seats plains kirin cubwarden =
  let base = S.landsFor plains S.alice 10 (Setup.emptyGame seats)
      (withKirin, kirinId) = S.handOne kirin base
      (board, spellId) = S.handOne cubwarden withKirin
      down =
        S.runPure
          S.identityAnswer
          board
          (Cast.castSpell S.manaPerformer S.alice kirinId (S.printingName kirin) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
   in fmap (\host -> (host, down, spellId)) (faceDownOn down)

-- The one face-down permanent alice OWNS -- Game.zoneMembers indexes the
-- battlefield by owner (CR 108.3) -- found by its status rather than by a name:
-- CR 708.2a has left it with none to search for.
faceDownOn :: GameState.GameState -> Maybe ObjectId.ObjectId
faceDownOn gs =
  List.find
    (\oid -> maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject oid gs))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- Cast Cubwarden for its mutate cost at `host`, put it on `side`, and drain the
-- stack: the spell first, then CR 702.140d's trigger, which CR 603.3 puts on the
-- stack at the next time a player would receive priority.
merging :: MutateSide.MutateSide -> ObjectId.ObjectId -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
merging side host = mergingWith (mutatingAt side host)

-- `merging` under this answerer.
mergingWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
mergingWith answer board spellId =
  let cast = S.runPure answer board (S.cast S.alice spellId)
   in -- SIX passes over a stack of two, so every assertion reads a DRAINED
      -- stack: capping the resolutions at the expected count would let a
      -- trigger a wrong implementation added sit there unresolved, and the
      -- assertion that its effect did not happen would pass for that reason.
      -- Stack.resolveTop over an empty stack is a no-op.
      S.runPure answer cast (Monad.replicateM_ 6 (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)

-- CR 601.2b's announcement answered by NAMING the mutate cost rather than an
-- index, and CR 601.2c's target by FILTERING the offered set rather than
-- building a recipient: an answerer that hands back a Recipient.ToObject of the
-- same permanent is a different recipient, and CR 608.2b's re-read drops it
-- silently. CR 702.140c's side is the one thing two runs of this differ by.
mutatingAt :: MutateSide.MutateSide -> ObjectId.ObjectId -> Prompt.Prompt r -> r
mutatingAt side host p = case p of
  Prompt.ChooseCost _ _ _ candidates _ ->
    Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((`elem` [Just mutateCost, Just graftCost]) . Cost.Type.mana . CandidateCost.cost) candidates)
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just host) . Recipient.objectOf) . snd) sets
  Prompt.ChooseMutateSide {} -> side
  _ -> S.identityAnswer p

-- The copy case's answerer: Cubwarden's mutate cost named rather than indexed,
-- and the two target slots told apart BY NAME. Rule 702.140a's own slot is
-- filtered down to `host` -- CR 608.2b's re-read is what makes a hand-built
-- recipient the wrong answer -- and Double Major's is taken as offered, since a
-- creature spell on the stack is a new object (CR 400.7) whose id no caller
-- holds, and rule 702.140c's Cubwarden is the only one there.
doublingAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
doublingAt host p = case p of
  Prompt.ChooseCost _ _ _ candidates _ ->
    Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just mutateCost) . Cost.Type.mana . CandidateCost.cost) candidates)
  Prompt.ChooseTargets _ _ _ sets ->
    Map.mapWithKey
      (\slot (_, offered) -> if slot == Card.mutateSlot then Set.filter ((== Just host) . Recipient.objectOf) offered else offered)
      sets
  Prompt.ChooseMutateSide {} -> MutateSide.Over
  _ -> S.identityAnswer p

-- The components of one permanent, top first, as the KIND of component each is
-- beside the name its printing carries. `componentNames` above cannot tell a
-- card from a token interned under the same printing.
componentKinds :: ObjectId.ObjectId -> GameState.GameState -> [(Text.Text, CardName.CardName)]
componentKinds oid gs =
  foldMap
    (Maybe.mapMaybe (\component -> fmap ((,) (componentKind component) . S.nameOf . Printing.card) (Game.printingOf (Game.printingOfComponent component) gs)) . Foldable.toList . Game.componentsOf . Object.source)
    (Game.lookupObject oid gs)

-- Which of CR 731.2's nouns a component is, named so an assertion can read it.
componentKind :: MergeComponent.MergeComponent -> Text.Text
componentKind component = case component of
  MergeComponent.OfCard _ -> Text.pack "OfCard"
  MergeComponent.OfToken _ -> Text.pack "OfToken"
  MergeComponent.OfMeld _ -> Text.pack "OfMeld"

-- Cubwarden's mutate cost, {2}{W}{W}, written as the announcement names it.
mutateCost :: ManaCost.ManaCost
mutateCost = ManaCost.MkManaCost [ManaSymbol.Generic 2, theWhite, theWhite]

-- Synthetic Graftling's, {G}.
graftCost :: ManaCost.ManaCost
graftCost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]

theWhite :: ManaSymbol.ManaSymbol
theWhite = ManaSymbol.OfType (ManaType.Colored Color.White)

-- Tok-Tok, Volcano Born's printed keyword, which the flip cases read to tell the
-- alternative half apart from the normal one.
protectionFromRed :: Keyword.Keyword
protectionFromRed = Keyword.Protection Protection.MkProtection {Protection.quality = Filter.Type.HasColor Color.Red, Protection.spares = Nothing}

-- What CR 731.3's split puts into alice's graveyard, by name.
graveyardNames :: GameState.GameState -> [CardName.CardName]
graveyardNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Graveyard S.alice gs)

-- The same read one zone over, which is where CR 701.24's shuffle puts a card
-- that answered its own put-into-a-graveyard trigger.
libraryNames :: GameState.GameState -> [CardName.CardName]
libraryNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Library S.alice gs)

-- The cards representing one permanent, top first, by name -- CR 731.2's order,
-- read through the classifier CR 713.8 and CR 731.3 share.
componentNames :: ObjectId.ObjectId -> GameState.GameState -> [CardName.CardName]
componentNames oid gs =
  foldMap
    (Maybe.mapMaybe (\component -> fmap (S.nameOf . Printing.card) (Game.printingOf (Game.printingOfComponent component) gs)) . Foldable.toList . Game.componentsOf . Object.source)
    (Game.lookupObject oid gs)

-- The first of the two Cat tokens CR 702.140d's trigger created, which is the
-- token these cases merge with. By name, so nothing else on the board can stand
-- in for it.
catId :: GameState.GameState -> ObjectId.ObjectId
catId gs =
  Maybe.fromMaybe (ObjectId.MkObjectId 999) $
    List.find
      (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Cat Token")))
      (Game.zoneMembers Zone.Battlefield S.alice gs)

-- The printing a source names when it is a bare card, and nothing otherwise --
-- what the commander case above needs before it can rebuild the source as a
-- merge of that same printing twice.
printingBehind :: Source.Source -> Maybe PrintingId.PrintingId
printingBehind source = case source of
  Source.OfCard pid -> Just pid
  _ -> Nothing

-- One permanent rewritten into a merged permanent holding a CARD and a TOKEN
-- under the SAME printing, which is what makes the two readings of rule
-- 903.9c's split differ. Written by hand because no card in data/cards/ mints a
-- token copy of a card, so no merge in this file's other cases can assemble the
-- pair.
asMergeOfCardAndToken :: ObjectId.ObjectId -> PrintingId.PrintingId -> GameState.GameState -> GameState.GameState
asMergeOfCardAndToken oid pid gs =
  gs
    { GameState.objects =
        Map.adjust
          (\o -> o {Object.source = Source.OfMerge (fmap (\c -> ShownComponent.MkShownComponent c Nothing Facing.FaceUp) (MergeComponent.OfCard (Game.componentCardOf o pid) NonEmpty.:| [MergeComponent.OfToken pid]))})
          oid
          (GameState.objects gs)
    }

-- CR 903.9b's offer accepted, and nothing else answered: the move below raises
-- no other prompt, so an answerer that took one would be hiding a question.
returningCommander :: Prompt.Prompt r -> r
returningCommander p = case p of
  Prompt.ReturnCommander {} -> CommandZoneDecision.Returns
  _ -> S.identityAnswer p

-- The names each CR 708.9 departure reveal showed, in log order.
departureRevealNames :: GameState.GameState -> [Set.Set CardName.CardName]
departureRevealNames gs =
  [ PC.names (Revealed.characteristics r)
  | GameEvent.Revealed r <- S.eventsOf gs,
    Revealed.cause r `elem` [RevealCause.LeavingFaceDown, RevealCause.LeavingFaceDownComponent]
  ]

-- Each CR 708.9 departure reveal on the log: revealer, object, and cause.
departureReveals :: GameState.GameState -> [(PlayerId.PlayerId, ObjectId.ObjectId, RevealCause.RevealCause)]
departureReveals gs =
  [ (Revealed.player r, Revealed.card r, Revealed.cause r)
  | GameEvent.Revealed r <- S.eventsOf gs,
    Revealed.cause r `elem` [RevealCause.LeavingFaceDown, RevealCause.LeavingFaceDownComponent]
  ]
