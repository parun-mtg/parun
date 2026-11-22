{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Covers Parun.Play.Search, Parun.Play.Polarity, Parun.Engine.Engine.resumePriority and
-- Parun.Engine.Engine.resumeTurnBasedAction: a sampled world, a restarted
-- priority decision or combat declaration, and playouts of one decision.
module Parun.SearchSpec where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Word as Word
import Numeric.Natural (Natural)
import qualified Parun.BotSpec as BotSpec
import qualified Parun.Engine.Card as Card
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Engine.Game
import qualified Parun.Engine.Modal as Engine.Modal
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Polarity as Polarity
import qualified Parun.Play.Search as Search
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Concession as Concession
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Features as Features
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.KickerDecision as KickerDecision
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PayOffer as PayOffer
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Placement as Placement
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.Polarity as Polarity.Type
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Program as Program
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.RevealCause as RevealCause
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.SearchMethod as SearchMethod
import qualified Parun.Types.Seat as Seat
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Source as Source
import qualified Parun.Types.Staged as Staged
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Weights as Weights
import qualified Parun.Types.Zone as Zone

spec :: forall m n. (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Search" $ do
  Spec.it s "CR 400.2 a sampled world looks the same to its viewer, its unseen cards dealt anew" $ do
    (gs, _) <- decision s registry
    worlds <- traverse (\n -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.sample Search.known Search.blank viewer gs) (S.seeded n))) [1 .. 8 :: Word.Word32]
    Foldable.for_ worlds $ \world -> do
      Spec.assertEqWith s "the viewer's view" (PlayerView.viewFor viewer world) (PlayerView.viewFor viewer gs)
      Spec.assertEqWith s "the opponent's unseen cards, as a whole" (List.sort (sources world (Search.unseen Search.blank viewer gs S.bob))) (List.sort (sources gs (Search.unseen Search.blank viewer gs S.bob)))
    Spec.assertBool s (any (\world -> handOf world S.bob /= handOf gs S.bob) worlds) "some world deals the opponent another hand"

  Spec.it s "a meta's posterior moves to the deck playing a card the opponent showed, once they show it" $ do
    (bear, meta) <- bearsOrCoursers s registry
    after <- showing s registry (Map.member bear)
    Spec.assertEqWith s "the opponent's cards shown and unseen, their 20 once each" (sum (Search.seen Search.blank viewer after S.bob) + Natural.length (Search.unseen Search.blank viewer after S.bob)) 20
    before <- showing s registry (\shown -> not (Map.null shown) && Map.notMember bear shown)
    let weights gs = fmap fst (Search.posterior meta (Search.seen Search.blank viewer gs S.bob))
    Spec.assertEqWith s "the weights, only Forests shown" (weights before) (1 / 2 NonEmpty.:| [1 / 2])
    Spec.assertEqWith s "the weights, a Grizzly Bears shown" (weights after) (0 NonEmpty.:| [1])

  Spec.it s "CR 400.2 a world sampled from a meta looks the same to its viewer, an opponent's unseen cards dealt from a deck explaining what they showed" $ do
    (bear, meta) <- bearsOrCoursers s registry
    before <- showing s registry (\shown -> not (Map.null shown) && Map.notMember bear shown)
    after <- showing s registry (Map.member bear)
    let from decks gs = traverse (\n -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.sample (Search.inferred decks) Search.blank viewer gs) (S.seeded n))) [1 .. 8 :: Word.Word32]
        worlds = from meta
        bears = snd (NonEmpty.last meta)
        rest gs = Map.differenceWith (\k n -> if k > n then Just (k - n) else Nothing) (Deck.cards bears) (Search.seen Search.blank viewer gs S.bob)
    early <- worlds before
    late <- worlds after
    Foldable.for_ [(world, gs) | (gs, sampled) <- [(before, early), (after, late)], world <- sampled] $ \(world, gs) ->
      Spec.assertEqWith s "the viewer's view" (PlayerView.viewFor viewer world) (PlayerView.viewFor viewer gs)
    Foldable.for_ late $ \world -> do
      Spec.assertEqWith s "the opponent's unseen cards, the bears deck less what they showed" (List.sort (printingsOf world (Search.unseen Search.blank viewer after S.bob))) (Search.spread (rest after))
    coursersOnly <- from (NonEmpty.head meta NonEmpty.:| []) before
    Foldable.for_ coursersOnly $ \world ->
      Spec.assertEqWith s "the viewer's own unseen cards, as a whole, from a meta without their deck" (List.sort (sources world (Search.unseen Search.blank viewer before viewer))) (List.sort (sources before (Search.unseen Search.blank viewer before viewer)))
    courser <- S.printingOf s registry "Centaur Courser"
    Spec.assertBool s (any (\world -> elem courser (printingsOf world (Search.unseen Search.blank viewer before S.bob))) early) "some world, only Forests shown, deals the opponent a Centaur Courser"

  Spec.it s "a card the viewer saw bounced to its owner's hand keeps being dealt there" $ do
    (gs, _) <- decision s registry
    let theirs oid = fmap Object.owner (Engine.Game.lookupObject oid gs) == Just S.bob && not (null (printingsOf gs [oid]))
    oid <- maybe (Spec.assertFailure s "the opponent has a card on the battlefield") pure (List.find theirs (Set.toList (GameState.battlefield gs)))
    let bounced = S.runPure S.identityAnswer gs (Event.changeZone Settle.none oid Zone.Hand)
        returned = filter (`notElem` handIds gs S.bob) (handIds bounced S.bob)
        memory = Search.remember viewer bounced (Search.remember viewer gs Search.blank)
        from remembered n = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.sample Search.known remembered viewer bounced) (S.seeded n))
    remembering <- traverse (from memory) [1 .. 8 :: Word.Word32]
    Foldable.for_ remembering $ \world -> do
      Spec.assertEqWith s "the viewer's view" (PlayerView.viewFor viewer world) (PlayerView.viewFor viewer bounced)
      Spec.assertEqWith s "the card returned" (sources world returned) (sources bounced returned)
    forgetting <- traverse (from Search.blank) [1 .. 8 :: Word.Word32]
    Spec.assertBool s (any (\world -> sources world returned /= sources bounced returned) forgetting) "forgetting it, some world deals another card there"
    Spec.assertEqWith s "the opponent's cards seen, the bounced one still among them" (Search.seen memory viewer bounced S.bob) (Search.seen Search.blank viewer gs S.bob)
    let shuffledIn = S.runPure S.identityAnswer bounced (Foldable.traverse_ (\o -> Event.changeZone Settle.none o Zone.Library) returned >> Event.shuffleLibrary S.bob)
        mixed = Search.remember viewer shuffledIn memory
    Spec.assertEqWith s "CR 701.24a put into the library and shuffled, it is no longer followed" (Search.pinned mixed viewer shuffledIn S.bob) []
    Spec.assertEqWith s "CR 701.24a and still counts among the opponent's cards" (Search.seen mixed viewer shuffledIn S.bob) (Search.seen Search.blank viewer gs S.bob)

  Spec.it s "CR 701.24a a card the viewer saw revealed and shuffled into its owner's library still counts among theirs, for the meta's posterior and the deal" $ do
    (bear, meta) <- bearsOrCoursers s registry
    before <- showing s registry (\shown -> not (Map.null shown) && Map.notMember bear shown)
    let library = Foldable.toList (Map.findWithDefault mempty S.bob (GameState.library before))
    oid <- maybe (Spec.assertFailure s "a Grizzly Bears in the opponent's library") pure (List.find (\o -> printingsOf before [o] == [bear]) library)
    let revealed = S.runPure S.identityAnswer before (Event.reveal RevealCause.Ordinary S.bob oid)
        shuffled = S.runPure S.identityAnswer revealed (Event.shuffleLibrary S.bob)
        memory = Search.remember viewer shuffled (Search.remember viewer revealed (Search.remember viewer before Search.blank))
        weights remembered = fmap fst (Search.posterior meta (Search.seen remembered viewer shuffled S.bob))
        bears = snd (NonEmpty.last meta)
        rest = Map.differenceWith (\k n -> if k > n then Just (k - n) else Nothing) (Deck.cards bears) (Search.seen memory viewer shuffled S.bob)
    Spec.assertBool s (Map.notMember oid (GameState.objects shuffled)) "the shuffle re-mints the card revealed"
    Spec.assertEqWith s "the Grizzly Bears, loose among the opponent's" (Map.lookup S.bob (Search.loose memory)) (Just (Map.singleton bear 1))
    Spec.assertEqWith s "the weights, remembering it" (weights memory) (0 NonEmpty.:| [1])
    Spec.assertEqWith s "the weights, forgetting it" (weights Search.blank) (1 / 2 NonEmpty.:| [1 / 2])
    worlds <- traverse (\n -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.sample (Search.inferred meta) memory viewer shuffled) (S.seeded n))) [1 .. 8 :: Word.Word32]
    Foldable.for_ worlds $ \world ->
      Spec.assertEqWith s "the opponent's unseen cards, the bears deck less what they showed, the Grizzly Bears dealt among them" (List.sort (printingsOf world (Search.unseen memory viewer shuffled S.bob))) (List.sort (bear : Search.spread rest))
    let library' = Foldable.toList (Map.findWithDefault mempty S.bob (GameState.library shuffled))
    again <- maybe (Spec.assertFailure s "a Grizzly Bears in the shuffled library") pure (List.find (\o -> printingsOf shuffled [o] == [bear]) library')
    let shownAgain = S.runPure S.identityAnswer shuffled (Event.reveal RevealCause.Ordinary S.bob again)
        recalled = Search.remember viewer shownAgain memory
    Spec.assertEqWith s "a Grizzly Bears shown again, counted once" (Search.seen recalled viewer shownAgain S.bob) (Search.seen memory viewer shuffled S.bob)
    top <- maybe (Spec.assertFailure s "a card atop the viewer's library") pure (Maybe.listToMaybe (Foldable.toList (Map.findWithDefault mempty viewer (GameState.library before))))
    let ownShown = S.runPure S.identityAnswer before (Event.reveal RevealCause.Ordinary viewer top)
        ownShuffled = S.runPure S.identityAnswer ownShown (Event.shuffleLibrary viewer)
        drew = S.runPure S.identityAnswer ownShuffled (Event.drawCard Settle.none viewer)
        shownMemory = Search.remember viewer ownShown (Search.remember viewer before Search.blank)
        shuffledMemory = Search.remember viewer ownShuffled shownMemory
        looseOf remembered = sum [n | printing <- printingsOf before [top], n <- Foldable.toList (Map.lookup printing (Map.findWithDefault Map.empty viewer (Search.loose remembered)))]
    Spec.assertEqWith s "the viewer's own card, drawn once shuffled atop its library, loosened then taken back" (looseOf shuffledMemory - looseOf shownMemory, looseOf shuffledMemory - looseOf (Search.remember viewer drew shuffledMemory)) (1, 1)

  Spec.it s "each player's memory is brought up to the game before a prompt is answered, from nothing once the log no longer extends what it read" $ do
    (gs, memories) <- recalling s registry
    Spec.assertEqWith s "the log read, for each player" (fmap Search.logged memories) (Map.fromList [(pid, Seq.length (Search.logOf gs)) | pid <- Map.keys (GameState.players gs)])
    hiddenCard <- maybe (Spec.assertFailure s "a card in the opponent's library") pure (Maybe.listToMaybe (Search.hidden viewer gs S.bob))
    let stale = Search.blank {Search.remembered = Map.fromList [(hiddenCard, (S.bob, Zone.Library, printing)) | printing <- printingsOf gs [hiddenCard]], Search.logged = 1, Search.lastRead = Nothing}
    Spec.assertEqWith s "a memory of a log this game's does not extend, brought up to it, read from nothing" (Search.remember viewer gs stale) (Search.remember viewer gs Search.blank)

  Spec.it s "an opponent's pass in their main phase, a land played and two untapped, weighs worlds dealing them a Grizzly Bears to cast less" $
    fewerBears s registry 10 (\past action -> action == Action.Pass && landPlayed past && length (untappedLands past) >= 2) False

  Spec.it s "an opponent's cast in their main phase, a land played, weighs worlds dealing them another Grizzly Bears to cast less, the card cast theirs in every one" $
    fewerBears s registry 11 (\past action -> case action of Action.Cast {} -> landPlayed past; _ -> False) True

  Spec.it s "a restarted priority decision asks its holder again, in the same step, offering the same menu" $ do
    (gs, menu) <- decision s registry
    Spec.assertEqWith s "the step and the menu asked again" (asksAgain (State.runStateT Engine.resumePriority gs)) (Just (TurnRegisters.phase (GameState.turn gs), menu))

  Spec.it s "two playouts of one decision share no state" $ do
    (gs, menu) <- decision s registry
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 1000, SearchBudget.prompts = 400, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = SearchMethod.Flat}
        playout option n = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget Weights.neutral Engine.resumePriority viewer [Search.acting viewer option] gs) (S.seeded n))
    case Search.options menu of
      one : two : _ -> do
        first <- playout one 3
        _ <- playout two 3
        again <- playout one 3
        Spec.assertEqWith s "the first option's playout again" again first
      _ -> Spec.assertFailure s "a decision offers two options"

  Spec.it s "a playout is cut off once it begins the turn after its decision's, short of a result" $ do
    (gs, menu) <- decision s registry
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 0, SearchBudget.prompts = 100000, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = SearchMethod.Flat}
    scores <- traverse (\(option, n) -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget Weights.neutral Engine.resumePriority viewer [Search.acting viewer option] gs) (S.seeded n))) (zip (Search.options menu) [1 ..])
    Spec.assertBool s (all (\score -> abs score < Search.won) scores) ("every playout scored short of a result: " <> show scores)

  Spec.it s "a cut-off playout scores its position by the weights, each side's features weighted" $ do
    (gs, attacker) <- capturing s registry 3 "a declaration of attackers" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareAttackers decider pid candidates | Search.searchable asked decider pid (Search.attacks candidates) -> Just (Asked.game asked, pid)
      _ -> Nothing
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 1000, SearchBudget.prompts = 0, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = SearchMethod.Flat}
        only side feature = Weights.MkWeights {Weights.ours = if side then feature else zero, Weights.theirs = if side then zero else feature}
        scored weights = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget weights Engine.resumeTurnBasedAction attacker [Search.attacking attacker []] gs) (S.seeded 1))
        controlled pid = [oid | oid <- Set.toList (GameState.battlefield gs), Projection.View.controllerOf oid gs == Just pid]
        creatures pid = [oid | oid <- controlled pid, Set.member CardType.Creature (ProjectedCharacteristics.cardTypes (Projection.project oid gs))]
        counts pid =
          [ ("life", zero {Features.life = 1}, maybe 0 Player.life (Map.lookup pid (GameState.players gs))),
            ("hand", zero {Features.hand = 1}, toInteger (length (handOf gs pid))),
            ("power", zero {Features.power = 1}, sum [max 0 (Maybe.fromMaybe 0 (Projection.powerOf oid gs)) | oid <- creatures pid]),
            ("toughness", zero {Features.toughness = 1}, sum [max 0 (Maybe.fromMaybe 0 (Projection.toughnessOf oid gs)) | oid <- creatures pid]),
            ("lands", zero {Features.lands = 1}, toInteger (length [oid | oid <- controlled pid, Set.member CardType.Land (ProjectedCharacteristics.cardTypes (Projection.project oid gs))])),
            ("permanents", zero {Features.permanents = 1}, toInteger (length (controlled pid)))
          ]
    Spec.assertBool s (not (null (creatures attacker))) "the attacker controls a creature"
    Foldable.for_ (counts attacker) $ \(name, feature, count) -> do
      got <- scored (only True feature)
      Spec.assertEqWith s ("our " <> name) got count
    Foldable.for_ (foldMap counts (filter (/= attacker) (Map.keys (GameState.players gs)))) $ \(name, feature, count) -> do
      got <- scored (only False feature)
      Spec.assertEqWith s ("their " <> name) got (negate count)

  Spec.it s "CR 508.1 a restarted declaration of attackers asks its declarer again, offering the same creatures" $ do
    (gs, offered) <- capturing s registry 3 "a declaration of attackers" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareAttackers decider pid candidates | Search.searchable asked decider pid (Search.attacks candidates) -> Just (Asked.game asked, (pid, candidates))
      _ -> Nothing
    Spec.assertEqWith s "the declarer and the creatures asked again" (declaresAgain (State.runStateT Engine.resumeTurnBasedAction gs)) (Just (Left offered))

  Spec.it s "CR 509.1 a restarted declaration of blockers asks its declarer again, offering the same creatures" $ do
    (gs, offered) <- capturing s registry 3 "a declaration of blockers" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareBlockers decider pid blockers attackers | Search.firstBlocks pid (Asked.game asked) && Search.searchable asked decider pid (Search.blocks blockers attackers) -> Just (Asked.game asked, (pid, blockers, attackers))
      _ -> Nothing
    Spec.assertEqWith s "the declarer and the creatures asked again" (declaresAgain (State.runStateT Engine.resumeTurnBasedAction gs)) (Just (Right offered))

  Spec.it s "the search bot attacks an opponent at 1 life who has nothing untapped" $ do
    (_, asked) <- openAttack s registry
    declared <- traverse (searched s asked) [1 .. 4]
    Spec.assertBool s (not (any null declared)) "attacked in every seed"

  Spec.it s "the information-set search bot declines an attack into an untapped blocker and open mana a pump spell in hand may punish, which the flat one makes" $ do
    asked <- intoOpenMana s registry
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 48, SearchBudget.turns = 0, SearchBudget.prompts = 80, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
        weights = Weights.neutral {Weights.theirs = (Weights.theirs Weights.neutral) {Features.life = 4}}
    flatly <- traverse (searchedAt s (budget SearchMethod.Flat) weights asked) [1 .. 4]
    Spec.assertBool s (not (any null flatly)) ("the flat bot attacked in every seed: " <> show flatly)
    treed <- traverse (searchedAt s (budget SearchMethod.InformationSet) weights asked) [1 .. 4]
    Spec.assertBool s (all null treed) ("the information-set bot attacked in no seed: " <> show treed)

  Spec.it s "CR 601.2c the search bot casting Shock with an opponent at 2 life aims it at them, flat or by an information-set tree" $ do
    gs <- shockAtTwo s registry
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
    flatly <- traverse (fmap (fmap recipientsOf) . linedIn s (budget SearchMethod.Flat) gs) [1 .. 4]
    Spec.assertEqWith s "Shock at Bob in every seed, flat" flatly (Just (Set.singleton (Recipient.ToPlayer S.bob)) <$ flatly)
    treed <- traverse (fmap (fmap recipientsOf) . linedIn s (budget SearchMethod.InformationSet) gs) [1 .. 4]
    Spec.assertEqWith s "Shock at Bob in every seed, by the tree" treed (Just (Set.singleton (Recipient.ToPlayer S.bob)) <$ treed)

  Spec.it s "CR 601.2b the search bot casting Abrade chooses its damage mode and aims it at the Grizzly Bears rather than destroy the Ornithopter, flat or by an information-set tree" $ do
    (gs, bears) <- abradeAtBears s registry
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 4, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
        expected = Just ([Seq.singleton (ModeIndex.MkModeIndex 0)], [] :: [Natural], Set.singleton (Recipient.ToCreature bears))
    flatly <- traverse (fmap (fmap answersOf) . linedIn s (budget SearchMethod.Flat) gs) [1 .. 4]
    Spec.assertEqWith s "Abrade's damage at the Bears in every seed, flat" flatly (expected <$ flatly)
    treed <- traverse (fmap (fmap answersOf) . linedIn s (budget SearchMethod.InformationSet) gs) [1 .. 4]
    Spec.assertEqWith s "Abrade's damage at the Bears in every seed, by the tree" treed (expected <$ treed)

  Spec.it s "CR 601.2c the search bot casting Forked Bolt with an opponent at 2 life announces one target and aims it at them, flat or by an information-set tree" $ do
    gs <- forkedBoltAtTwo s registry
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
        expected = Just ([], [1 :: Natural], Set.singleton (Recipient.ToPlayer S.bob))
    flatly <- traverse (fmap (fmap answersOf) . linedIn s (budget SearchMethod.Flat) gs) [1 .. 4]
    Spec.assertEqWith s "one target, Bob, in every seed, flat" flatly (expected <$ flatly)
    treed <- traverse (fmap (fmap answersOf) . linedIn s (budget SearchMethod.InformationSet) gs) [1 .. 4]
    Spec.assertEqWith s "one target, Bob, in every seed, by the tree" treed (expected <$ treed)

  Spec.it s "CR 601.2b the search bot casting Fireblast with both its costs payable and an opponent at 4 life aims it at them past its choice of cost, flat or by an information-set tree" $ do
    gs <- burnAtLife s registry "Fireblast" 6 4
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
        expected = Just (True, Set.singleton (Recipient.ToPlayer S.bob))
        costed line = (not (null [() | Search.Costed _ <- line]), recipientsOf line)
    flatly <- traverse (fmap (fmap costed) . linedIn s (budget SearchMethod.Flat) gs) [1 .. 4]
    Spec.assertEqWith s "a cost chosen, then Bob, in every seed, flat" flatly (expected <$ flatly)
    treed <- traverse (fmap (fmap costed) . linedIn s (budget SearchMethod.InformationSet) gs) [1 .. 4]
    Spec.assertEqWith s "a cost chosen, then Bob, in every seed, by the tree" treed (expected <$ treed)

  Spec.it s "CR 601.2b the search bot casting Burst Lightning with five Mountains and an opponent at 4 life kicks it and aims it at them, flat or by an information-set tree" $ do
    gs <- burnAtLife s registry "Burst Lightning" 5 4
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
        expected = Just ([1 :: Natural], Set.singleton (Recipient.ToPlayer S.bob))
        kicked line = ([KickerDecision.unwrap times | Search.Kicked _ _ times <- line], recipientsOf line)
    flatly <- traverse (fmap (fmap kicked) . linedIn s (budget SearchMethod.Flat) gs) [1 .. 4]
    Spec.assertEqWith s "kicked once, at Bob, in every seed, flat" flatly (expected <$ flatly)
    treed <- traverse (fmap (fmap kicked) . linedIn s (budget SearchMethod.InformationSet) gs) [1 .. 4]
    Spec.assertEqWith s "kicked once, at Bob, in every seed, by the tree" treed (expected <$ treed)

  Spec.it s "CR 107.3a the search bot casting Blaze with four Mountains and an opponent at 3 life announces X = 3 and aims it at them, flat or by an information-set tree" $ do
    gs <- burnAtLife s registry "Blaze" 4 3
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
        expected = Just ([3 :: Natural], Set.singleton (Recipient.ToPlayer S.bob))
        valued line = ([value | Search.Valued _ value <- line], recipientsOf line)
    flatly <- traverse (fmap (fmap valued) . linedIn s (budget SearchMethod.Flat) gs) [1 .. 4]
    Spec.assertEqWith s "X = 3, at Bob, in every seed, flat" flatly (expected <$ flatly)
    treed <- traverse (fmap (fmap valued) . linedIn s (budget SearchMethod.InformationSet) gs) [1 .. 4]
    Spec.assertEqWith s "X = 3, at Bob, in every seed, by the tree" treed (expected <$ treed)

  Spec.it s "CR 601.2g the search bot with a Plains, an Island and a Forest casts Volunteer Militia tapping the Plains alone, and taps nothing for mana at priority, flat or by an information-set tree" $ do
    gs <- militiaOnThreeLands s registry
    let budget method = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = method}
    flatly <- traverse (alicesTurn s (budget SearchMethod.Flat) gs) [1 .. 4]
    Spec.assertEqWith s "the Militia cast, one land tapped at most, in every seed, flat" flatly (Just (True, 1) <$ flatly)
    treed <- traverse (alicesTurn s (budget SearchMethod.InformationSet) gs) [1 .. 4]
    Spec.assertEqWith s "the Militia cast, one land tapped at most, in every seed, by the tree" treed (Just (True, 1) <$ treed)

  Spec.it s "the search bot making a mistake at every decision takes the option scored next below the best: no attack on an opponent at 1 life who has nothing untapped" $ do
    (_, asked) <- openAttack s registry
    declared <- traverse (searchedMaking s 100 asked) [1 .. 4]
    Spec.assertBool s (all null declared) ("held back in every seed: " <> show declared)

  Spec.it s "parun play's bot seat is the search bot, attacking an opponent at 1 life who has nothing untapped" $ do
    (pid, asked) <- openAttack s registry
    let seat = PlayerId.unwrap pid + 1
        options = PlayOptions.none {PlayOptions.bots = Set.singleton seat}
        answered seed = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Command.answerer (S.fixtureRegistry []) (const (pure Nothing)) (pure Nothing) (const (pure ())) (pure Map.empty) Search.unkept options Search.known seat) asked) (S.seeded seed))
    declared <- traverse answered [1 .. 8]
    Spec.assertBool s (not (any null declared)) "attacked in every seed"

  Spec.it s "the search bot blocks a lone attacker at 1 life" $ do
    (gs, (decider, pid, blockers, attackers)) <- capturing s registry 3 "a declaration of blockers against one attacker" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareBlockers decider pid blockers attackers | length attackers == 1 && Search.firstBlocks pid (Asked.game asked) && Search.searchable asked decider pid (Search.blocks blockers attackers) -> Just (Asked.game asked, (decider, pid, blockers, attackers))
      _ -> Nothing
    declared <- traverse (searched s (Asked.MkAsked [] (atOneLife pid gs) (Prompt.DeclareBlockers decider pid blockers attackers))) [1 .. 4]
    Spec.assertBool s (all (\declaration -> all (\attacker -> any (Set.member attacker) (Map.elems declaration)) attackers) declared) "blocked in every seed"

  Spec.it s "the greedy policy plays a land where it may, else casts a permanent spell" $ do
    let greedily asked seed = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.greedy (S.fixtureRegistry [])) asked) (S.seeded seed))
    (_, landed) <- capturing s registry 3 "a menu offering a land play" $ \asked -> case Asked.prompt asked of
      Prompt.ChooseAction _ _ menu | any isPlay menu && any isCast menu -> Just (Asked.game asked, Asked.MkAsked [] (Asked.game asked) (Asked.prompt asked))
      _ -> Nothing
    played <- traverse (greedily landed) [1 .. 4]
    Spec.assertBool s (all isPlay played) ("a land played in every seed: " <> show played)
    (_, casting) <- capturing s registry 3 "a menu offering a cast and no land play" $ \asked -> case Asked.prompt asked of
      Prompt.ChooseAction _ _ menu | any isCast menu && not (any isPlay menu) -> Just (Asked.game asked, Asked.MkAsked [] (Asked.game asked) (Asked.prompt asked))
      _ -> Nothing
    cast <- traverse (greedily casting) [1 .. 4]
    Spec.assertBool s (all isCast cast) ("a spell cast in every seed: " <> show cast)

  Spec.it s "the greedy policy leaves a spell with no permanent type to the random pick" $ do
    bear <- S.printingOf s registry "Grizzly Bears"
    growth <- S.printingOf s registry "Giant Growth"
    forest <- S.printingOf s registry "Forest"
    let deck = Deck.fromCards (Map.fromList [(bear, 6), (growth, 6), (forest, 8)])
    (_, instant) <- capturingIn s (deck NonEmpty.:| [deck]) 3 "a menu offering only a cast with no permanent type" $ \asked -> case Asked.prompt asked of
      Prompt.ChooseAction _ _ menu
        | any isCast menu && not (any isPlay menu) && all (\action -> not (isCast action) || notPermanent (Asked.game asked) action) menu ->
            Just (Asked.game asked, Asked.MkAsked [] (Asked.game asked) (Asked.prompt asked))
      _ -> Nothing
    chosen <- traverse (\seed -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.greedy (S.fixtureRegistry [])) instant) (S.seeded seed))) [1 .. 8]
    Spec.assertBool s (elem Action.Pass chosen) ("passed in some seed: " <> show chosen)

  Spec.it s "an effect's polarity toward its target slot, by the card behind each arm" $ do
    let table =
          [ ("Lightning Bolt", "target", Just Polarity.Type.Harms),
            ("Murder", "target", Just Polarity.Type.Harms),
            ("Counterspell", "spell", Just Polarity.Type.Harms),
            ("Downpour", "target", Just Polarity.Type.Harms),
            ("Blatant Thievery", "target", Just Polarity.Type.Harms),
            ("Unsummon", "target", Just Polarity.Type.Harms),
            ("Ashen Powder", "target", Just Polarity.Type.Helps),
            ("Giant Growth", "target", Just Polarity.Type.Helps),
            ("Disfigure", "target", Just Polarity.Type.Harms),
            ("Critical Hit", "target", Just Polarity.Type.Helps),
            ("Battlegrowth", "target", Just Polarity.Type.Helps),
            ("Blight Rot", "target", Just Polarity.Type.Harms),
            ("Ancestral Recall", "target", Just Polarity.Type.Helps),
            ("Natural Spring", "target", Just Polarity.Type.Helps),
            ("Final Punishment", "target", Just Polarity.Type.Harms),
            ("Dreadwaters", "target", Just Polarity.Type.Harms),
            ("Deception", "target", Just Polarity.Type.Harms),
            ("Lightning Bolt", "other", Nothing)
          ]
    Foldable.for_ table $ \(name, slot, expected) -> do
      card <- S.cardOf s registry name
      let effects = Engine.Modal.allEffects (Face.spell (NonEmpty.head (Card.Type.faces card)))
      Spec.assertEqWith s (name <> "'s " <> slot) (Polarity.slotPolarity (SlotName.MkSlotName (Text.pack slot)) effects) expected

  Spec.it s "the polar policy aims a harmful spell's targets at the chooser's opponent" $
    aiming s registry "Lightning Bolt" (/=)

  Spec.it s "the polar policy aims a helpful spell's targets at the chooser's own side" $
    aiming s registry "Giant Growth" (==)

  Spec.it s "the classified policy chooses the mode whose targets its effect favours" $ do
    -- Boros Charm's caster controls no creature, so its double strike
    -- helps only an opponent's bear and its indestructible names no slot:
    -- the 4 damage to a player is the one mode a target favours.
    charm <- S.printingOf s registry "Boros Charm"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    bears <- fmap snd <$> BotSpec.bears s registry
    let caster = Deck.fromCards (Map.fromList [(charm, 12), (mountain, 12), (plains, 12)])
    (_, asked :: Asked.Asked GameState.GameState (Seq.Seq ModeIndex.ModeIndex)) <- capturingIn s (caster NonEmpty.:| [NonEmpty.last bears]) 3 "Boros Charm's modes with a creature to target" $ \asked -> case Asked.prompt asked of
      Prompt.ChooseModes decider chooser oid legal selection
        | Set.member (ModeIndex.MkModeIndex 2) legal -> Just (Asked.game asked, Asked.MkAsked [] (Asked.game asked) (Prompt.ChooseModes decider chooser oid legal selection))
      _ -> Nothing
    chosen <- traverse (\seed -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.classified (S.fixtureRegistry [])) asked) (S.seeded seed))) [1 .. 8]
    Spec.assertEqWith s "the damage mode in every seed" chosen (Seq.singleton (ModeIndex.MkModeIndex 0) <$ chosen)

  Spec.it s "the classified policy pays to keep its own spell from being countered" $ do
    spike <- S.printingOf s registry "Force Spike"
    island <- S.printingOf s registry "Island"
    bear <- S.printingOf s registry "Grizzly Bears"
    forest <- S.printingOf s registry "Forest"
    let caster = Deck.fromCards (Map.fromList [(spike, 12), (island, 24)])
        bears = Deck.fromCards (Map.fromList [(bear, 12), (forest, 24)])
    (_, asked :: Asked.Asked GameState.GameState PaymentDecision.PaymentDecision) <- capturingAmong s (caster NonEmpty.:| [bears]) [1 .. 16] "Force Spike's offer" $ \asked -> case Asked.prompt asked of
      prompt@(Prompt.ChooseToPay _ _ _ PayOffer.AtClause {} _ _) -> Just (Asked.game asked, Asked.MkAsked [] (Asked.game asked) prompt)
      _ -> Nothing
    chosen <- traverse (\seed -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.classified (S.fixtureRegistry [])) asked) (S.seeded seed))) [1 .. 8]
    Spec.assertEqWith s "paid in every seed" chosen (PaymentDecision.Pays <$ chosen)

  Spec.it s "a greedy playout plays a land at the decision it restarts" $ do
    (gs, pid) <- capturing s registry 3 "a menu offering a land play" $ \asked -> case Asked.prompt asked of
      Prompt.ChooseAction decider pid menu | any isPlay menu && Search.searchable asked decider pid (Search.options menu) -> Just (Asked.game asked, pid)
      _ -> Nothing
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 1000, SearchBudget.prompts = 3, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Greedy, SearchBudget.mistakes = 0, SearchBudget.method = SearchMethod.Flat}
        lands = Weights.MkWeights {Weights.ours = zero {Features.lands = 1}, Weights.theirs = zero}
        before = toInteger (length [oid | oid <- Set.toList (GameState.battlefield gs), Projection.View.controllerOf oid gs == Just pid, Set.member CardType.Land (ProjectedCharacteristics.cardTypes (Projection.project oid gs))])
    scores <- traverse (\n -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget lands Engine.resumePriority pid [] gs) (S.seeded n))) [1 .. 8]
    Spec.assertEqWith s "a land more in every seed" scores ((before + 1) <$ scores)
  where
    viewer = S.alice

-- The first targets announced for `spell` from turn 3 on whose every slot
-- offers recipients on both sides, in a game of decks of bears and `spell`,
-- answered by Search.polar in eight seeds: every target chosen lies on a side
-- `related` to the chooser.
aiming :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> (PlayerId.PlayerId -> PlayerId.PlayerId -> Bool) -> m ()
aiming s registry spell related = do
  bear <- S.printingOf s registry "Grizzly Bears"
  cast <- S.printingOf s registry spell
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  let deck = Deck.fromCards (Map.fromList [(bear, 16), (cast, 4), (mountain, 4), (forest, 8)])
      side gs recipient = Recipient.playerOf recipient <|> ((`Projection.View.controllerOf` gs) =<< Recipient.objectOf recipient)
      both gs chooser legal = any (\r -> side gs r == Just chooser) legal && any (maybe False (/= chooser) . side gs) legal
      named gs oid = Set.member (CardName.MkCardName (Text.pack spell)) (ProjectedCharacteristics.names (Projection.project oid gs))
  (gs, (chooser, asked :: Asked.Asked GameState.GameState (Map.Map SlotName.SlotName (Set.Set Recipient.Recipient)))) <- capturingIn s (deck NonEmpty.:| [deck]) 3 ("targets for " <> spell <> " on both sides") $ \asked -> case Asked.prompt asked of
    Prompt.ChooseTargets decider chooser oid slots
      | named (Asked.game asked) oid && all (both (Asked.game asked) chooser . snd) slots ->
          Just (Asked.game asked, (chooser, Asked.MkAsked [] (Asked.game asked) (Prompt.ChooseTargets decider chooser oid slots)))
    _ -> Nothing
  chosen <- traverse (\seed -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.polar (S.fixtureRegistry [])) asked) (S.seeded seed))) [1 .. 8]
  let sides = [side gs recipient | answer <- chosen, recipient <- foldMap Set.toList (Map.elems answer)]
  Spec.assertBool s (not (null sides) && all (maybe False (related chooser)) sides) ("every target on the side wanted: " <> show sides)

-- Seat 1's first priority decision with a choice from turn 3 on, in a seeded
-- game of the bears decks, the random bot in both seats: the game as asked
-- and the menu offered.
decision :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, [Action.Action])
decision s registry = capturing s registry 5 "a decision" $ \asked -> case Asked.prompt asked of
  Prompt.ChooseAction decider pid menu
    | pid == S.alice && Search.searchable asked decider pid (Search.options menu) -> Just (Asked.game asked, menu)
  _ -> Nothing

-- The first prompt from turn 3 on that `found` takes, in a game of the bears
-- decks from `seed`, the random bot in both seats answering everything else.
capturing :: forall m n x. (Monad m) => Spec.Spec m n -> Registry.Registry m -> Word.Word32 -> String -> (forall r. Asked.Asked GameState.GameState r -> Maybe (GameState.GameState, x)) -> m (GameState.GameState, x)
capturing s registry seed what found = do
  decks <- fmap snd <$> BotSpec.bears s registry
  capturingIn s decks seed what found

-- 'capturing' in a game of these decks.
capturingIn :: forall m n x. (Monad m) => Spec.Spec m n -> NonEmpty.NonEmpty Deck.Deck -> Word.Word32 -> String -> (forall r. Asked.Asked GameState.GameState r -> Maybe (GameState.GameState, x)) -> m (GameState.GameState, x)
capturingIn s decks seed = capturingAmong s decks [seed]

-- 'capturingIn' in the game of the first of `seeds` reaching the prompt.
capturingAmong :: forall m n x. (Monad m) => Spec.Spec m n -> NonEmpty.NonEmpty Deck.Deck -> [Word.Word32] -> String -> (forall r. Asked.Asked GameState.GameState r -> Maybe (GameState.GameState, x)) -> m (GameState.GameState, x)
capturingAmong s decks seeds what found = go seeds
  where
    go remaining = case remaining of
      [] -> Spec.assertFailure s ("no " <> what <> " from turn 3 on")
      seed : rest -> do
        (_, taken) <- State.runStateT (Game.run lifted Nothing Limits.standard [] (Map.fromList [(Command.idOf 1, capture), (Command.idOf 2, capture)]) (S.seeded seed) Map.empty (Command.seated 1 decks)) Nothing
        maybe (go rest) pure taken
    lifted :: Registry.Registry (State.StateT (Maybe (GameState.GameState, x)) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    capture :: Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT (Maybe (GameState.GameState, x)) m))
    capture = Answerer.MkAnswerer $ \asked -> case found asked of
      Just taken
        | TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) >= 3 -> do
            Game.underneath (State.put (Just taken))
            Game.stop (Text.pack "found")
      _ -> Answerer.unwrap (Game.bot lifted) asked

-- That the plays `afterPlay` finds from `seed` of those `taken` weigh the worlds of the
-- opponent holding fewer Grizzly Bears, as do the worlds 'Search.worlds'
-- deals weighing them, and, where `kept`, weigh no world 0, as the same
-- plays naming an object their menu does not offer weigh each.
fewerBears :: forall m n. (Monad m) => Spec.Spec m n -> Registry.Registry m -> Word.Word32 -> (GameState.GameState -> Action.Action -> Bool) -> Bool -> m ()
fewerBears s registry seed taken kept = do
  (gs, plays) <- afterPlay s registry seed taken
  bear <- S.printingOf s registry "Grizzly Bears"
  let played :: Game.Play m a -> Word.Word32 -> m a
      played act n = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT act (S.seeded n))
      heldBears world = toRational (length (filter (== bear) (printingsOf world (filter (`elem` handIds gs S.bob) (Search.unseen Search.blank viewer gs S.bob)))))
      mean weighed = sum [w * heldBears world | (w, world) <- weighed] / sum (fmap fst weighed)
  candidates <- traverse (played (Search.sample Search.known Search.blank viewer gs)) [1 .. 32]
  unweighed <- traverse (\(world, n) -> fmap (\w -> (w, world)) (played (Search.plausibility Search.known Search.blank viewer [] gs world) n)) (zip candidates [1 ..])
  weighed <- traverse (\(world, n) -> fmap (\w -> (w, world)) (played (Search.plausibility Search.known Search.blank viewer plays gs world) n)) (zip candidates [1 ..])
  Spec.assertEqWith s "every weight, no play watched" (fmap fst unweighed) (1 <$ candidates)
  Monad.when kept $ do
    Spec.assertBool s (all ((> 0) . fst) weighed) ("every world weighed above 0: " <> show (fmap (fromRational . fst) weighed :: [Double]))
    let unoffered = [(pid, past, Identity.runIdentity (Search.objectsIn (const (pure Search.unseenSlot)) action)) | (pid, past, action) <- plays]
    never <- traverse (\(world, n) -> played (Search.plausibility Search.known Search.blank viewer unoffered gs world) n) (zip candidates [1 ..])
    Spec.assertEqWith s "every weight, each play's action naming an object its menu does not offer" never (0 <$ candidates)
  Spec.assertBool s (mean weighed < mean unweighed) ("the Grizzly Bears held, weighed by the play (" <> show (fromRational (mean weighed) :: Double) <> ") fewer than not (" <> show (fromRational (mean unweighed) :: Double) <> ")")
  let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 0, SearchBudget.prompts = 0, SearchBudget.candidates = 32, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0, SearchBudget.method = SearchMethod.Flat}
      dealt watched = played (Search.worlds budget Search.known Search.blank watched viewer gs >>= Monad.replicateM 200) 1
  inferring <- dealt plays
  blind <- dealt []
  Spec.assertBool s (mean (fmap ((,) 1) inferring) < mean (fmap ((,) 1) blind)) ("the Grizzly Bears held in the worlds dealt, weighing the play (" <> show (fromRational (mean (fmap ((,) 1) inferring)) :: Double) <> ") fewer than not (" <> show (fromRational (mean (fmap ((,) 1) blind)) :: Double) <> ")")
  where
    viewer = S.alice

-- Seat 1's first priority decision with a choice from turn 3 on, in a game
-- of the bears decks from `seed` watched by Search.watching, the random bot
-- in both seats, once seat 2 has taken an action in their own main phase
-- with the stack empty that `taken` picks, given the game as they were
-- asked, and still holds a card they held then: the game as asked and the
-- plays kept that are such.
afterPlay :: forall m n. (Monad m) => Spec.Spec m n -> Registry.Registry m -> Word.Word32 -> (GameState.GameState -> Action.Action -> Bool) -> m (GameState.GameState, [Search.Watched])
afterPlay s registry seed taken = do
  decks <- fmap snd <$> BotSpec.bears s registry
  (_, (_, found)) <- State.runStateT (Game.run lifted Nothing Limits.standard [Search.watching (\f -> State.modify' (\(plays, found) -> let kept = f plays in kept `seq` (kept, found)))] (Map.fromList [(Command.idOf 1, capture), (Command.idOf 2, capture)]) (S.seeded seed) Map.empty (Command.seated 1 decks)) ([], Nothing)
  maybe (Spec.assertFailure s "no decision after an informative play") pure found
  where
    lifted :: Registry.Registry (State.StateT ([Search.Watched], Maybe (GameState.GameState, [Search.Watched])) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    informative gs play@(pid, past, action) =
      let turn = GameState.turn past
       in pid == S.bob
            && taken past action
            && TurnRegisters.activePlayer turn == pid
            && elem (TurnRegisters.phase turn) [Phase.PrecombatMain, Phase.PostcombatMain]
            && null (TurnRegisters.stack turn)
            && not (Search.stale gs play)
    capture :: Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT ([Search.Watched], Maybe (GameState.GameState, [Search.Watched])) m))
    capture = Answerer.MkAnswerer $ \asked -> do
      (plays, _) <- Game.underneath State.get
      case Asked.prompt asked of
        Prompt.ChooseAction decider pid menu
          | pid == S.alice,
            Search.searchable asked decider pid (Search.options menu),
            TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) >= 3,
            any (informative (Asked.game asked)) plays -> do
              Game.underneath (State.put (plays, Just (Asked.game asked, filter (informative (Asked.game asked)) plays)))
              Game.stop (Text.pack "found")
        _ -> Answerer.unwrap (Game.bot lifted) asked

-- Seat 1's first priority decision from turn 3 on, in a game of the bears
-- decks from seed 5 watched by Search.remembering, the random bot in both
-- seats: the game as asked and the memories kept then.
recalling :: forall m n. (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, Map.Map PlayerId.PlayerId Search.Memory)
recalling s registry = do
  decks <- fmap snd <$> BotSpec.bears s registry
  (_, (_, taken)) <- State.runStateT (Game.run lifted Nothing Limits.standard [Search.remembering (\f -> State.modify' (\(memories, taken) -> let kept = f memories in kept `seq` (kept, taken)))] (Map.fromList [(Command.idOf 1, capture), (Command.idOf 2, capture)]) (S.seeded 5) Map.empty (Command.seated 1 decks)) (Map.empty, Nothing)
  maybe (Spec.assertFailure s "no decision from turn 3 on") pure taken
  where
    lifted :: Registry.Registry (State.StateT (Map.Map PlayerId.PlayerId Search.Memory, Maybe (GameState.GameState, Map.Map PlayerId.PlayerId Search.Memory)) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    capture :: Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT (Map.Map PlayerId.PlayerId Search.Memory, Maybe (GameState.GameState, Map.Map PlayerId.PlayerId Search.Memory)) m))
    capture = Answerer.MkAnswerer $ \asked -> do
      (memories, _) <- Game.underneath State.get
      case Asked.prompt asked of
        Prompt.ChooseAction _ pid _
          | pid == S.alice,
            TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) >= 3 -> do
              Game.underneath (State.put (memories, Just (Asked.game asked, memories)))
              Game.stop (Text.pack "found")
        _ -> Answerer.unwrap (Game.bot lifted) asked

-- Whether seat 2 has played a land this turn, so none in their hand is
-- offered.
landPlayed :: GameState.GameState -> Bool
landPlayed gs = Map.findWithDefault 0 S.bob (GameState.landsPlayed gs) >= 1

-- The untapped lands seat 2 controls.
untappedLands :: GameState.GameState -> [ObjectId.ObjectId]
untappedLands gs =
  [ oid
  | oid <- Set.toList (GameState.battlefield gs),
    Projection.View.controllerOf oid gs == Just S.bob,
    Set.member CardType.Land (ProjectedCharacteristics.cardTypes (Projection.project oid gs)),
    fmap Object.tapped (Engine.Game.lookupObject oid gs) == Just TapState.Untapped
  ]

-- The ids in a player's hand.
handIds :: GameState.GameState -> PlayerId.PlayerId -> [ObjectId.ObjectId]
handIds gs pid = Foldable.toList (Map.findWithDefault mempty pid (GameState.hand gs))

-- A declaration of attackers from turn 3 on, every opponent of its declarer
-- at 1 life with every permanent tapped: the declarer and the prompt.
openAttack :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (PlayerId.PlayerId, Asked.Asked GameState.GameState [ObjectId.ObjectId])
openAttack s registry = do
  (gs, (decider, pid, candidates)) <- capturing s registry 3 "a declaration of attackers" $ \asked -> case Asked.prompt asked of
    Prompt.DeclareAttackers decider pid candidates | Search.searchable asked decider pid (Search.attacks candidates) -> Just (Asked.game asked, (decider, pid, candidates))
    _ -> Nothing
  let world = othersAtOneLife pid gs {GameState.objects = Map.map (\o -> if Object.owner o /= pid && Object.zone o == Zone.Battlefield then o {Object.tapped = TapState.Tapped} else o) (GameState.objects gs)}
  pure (pid, Asked.MkAsked [] world (Prompt.DeclareAttackers decider pid candidates))

-- Seat 1's declaration of attackers, offering its one Grizzly Bears, seat 2
-- controlling an untapped Grizzly Bears and Forest and holding a Blossoming
-- Defense and a Forest, with two more of each and a Grizzly Bears in its
-- library: CR 400.2 hides which two it holds.
intoOpenMana :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Asked.Asked GameState.GameState [ObjectId.ObjectId])
intoOpenMana s registry = do
  let cards :: [String] -> [Placement.Placement]
      cards = fmap S.cardSetup
      alice = (S.battlefield S.alice [S.settled "attacker" "Grizzly Bears", S.cardSetup "Forest"]) {Seat.library = Seq.fromList (cards (replicate 5 "Forest"))}
      bob = (S.battlefield S.bob [S.settled "blocker" "Grizzly Bears", S.cardSetup "Forest"]) {Seat.hand = Seq.fromList (cards ["Blossoming Defense", "Forest"]), Seat.library = Seq.fromList (cards ["Forest", "Blossoming Defense", "Forest", "Grizzly Bears", "Blossoming Defense"])}
  built <- S.buildBoardOrFail s registry (S.board (alice NonEmpty.:| [bob]) S.alice S.declareAttackers)
  attacker <- maybe (Spec.assertFailure s "no attacker") pure (Map.lookup (Label.MkLabel (Text.pack "attacker")) (Staged.objects built))
  pure (Asked.MkAsked [] (Staged.state built) (Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [attacker]))

-- Alice in her precombat main phase with an untapped Mountain and a Shock
-- in hand; Bob at 2 life with a Grizzly Bears, his hand empty: the game as
-- built.
shockAtTwo :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
shockAtTwo s registry = do
  let cards :: [String] -> [Placement.Placement]
      cards = fmap S.cardSetup
      alice = (S.battlefield S.alice [S.cardSetup "Mountain"]) {Seat.hand = Seq.fromList (cards ["Shock"]), Seat.library = Seq.fromList (cards (replicate 5 "Mountain"))}
      bob = (S.battlefield S.bob [S.settled "his" "Grizzly Bears"]) {Seat.life = 2, Seat.library = Seq.fromList (cards (replicate 5 "Forest"))}
  Staged.state <$> S.buildBoardOrFail s registry (S.board (alice NonEmpty.:| [bob]) S.alice S.precombatMain)

-- Alice's board for Abrade: two Mountains and Abrade in hand, Bob's
-- Grizzly Bears and Ornithopter, and the Bears' id.
abradeAtBears :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
abradeAtBears s registry = do
  let cards :: [String] -> [Placement.Placement]
      cards = fmap S.cardSetup
      alice = (S.battlefield S.alice (cards ["Mountain", "Mountain"])) {Seat.hand = Seq.fromList (cards ["Abrade"]), Seat.library = Seq.fromList (cards (replicate 5 "Mountain"))}
      bob = (S.battlefield S.bob [S.settled "bears" "Grizzly Bears", S.settled "thopter" "Ornithopter"]) {Seat.library = Seq.fromList (cards (replicate 5 "Forest"))}
  built <- S.buildBoardOrFail s registry (S.board (alice NonEmpty.:| [bob]) S.alice S.precombatMain)
  bears <- maybe (Spec.assertFailure s "no Grizzly Bears") pure (Map.lookup (Label.MkLabel (Text.pack "bears")) (Staged.objects built))
  pure (Staged.state built, bears)

-- Alice's board for Forked Bolt: a Mountain and Forked Bolt in hand, Bob at
-- 2 life with Grizzly Bears.
forkedBoltAtTwo :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
forkedBoltAtTwo s registry = do
  let cards :: [String] -> [Placement.Placement]
      cards = fmap S.cardSetup
      alice = (S.battlefield S.alice [S.cardSetup "Mountain"]) {Seat.hand = Seq.fromList (cards ["Forked Bolt"]), Seat.library = Seq.fromList (cards (replicate 5 "Mountain"))}
      bob = (S.battlefield S.bob [S.settled "his" "Grizzly Bears"]) {Seat.life = 2, Seat.library = Seq.fromList (cards (replicate 5 "Forest"))}
  Staged.state <$> S.buildBoardOrFail s registry (S.board (alice NonEmpty.:| [bob]) S.alice S.precombatMain)

-- Alice's board for a burn spell: `mountains` Mountains and the spell in
-- hand, Bob at `life` with Grizzly Bears, Alice holding priority.
burnAtLife :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Int -> Integer -> m GameState.GameState
burnAtLife s registry spell mountains life = do
  let cards :: [String] -> [Placement.Placement]
      cards = fmap S.cardSetup
      alice = (S.battlefield S.alice (cards (replicate mountains "Mountain"))) {Seat.hand = Seq.fromList (cards [spell]), Seat.library = Seq.fromList (cards (replicate 5 "Mountain"))}
      bob = (S.battlefield S.bob [S.settled "his" "Grizzly Bears"]) {Seat.life = life, Seat.library = Seq.fromList (cards (replicate 5 "Forest"))}
  gs <- Staged.state <$> S.buildBoardOrFail s registry (S.board (alice NonEmpty.:| [bob]) S.alice S.precombatMain)
  pure gs {GameState.turn = (GameState.turn gs) {TurnRegisters.priority = Just S.alice}}

-- Alice's board for Volunteer Militia: a Plains, an Island and a Forest
-- and the Militia in hand, Bob with no permanents, Alice holding priority.
militiaOnThreeLands :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
militiaOnThreeLands s registry = do
  let cards :: [String] -> [Placement.Placement]
      cards = fmap S.cardSetup
      alice = (S.battlefield S.alice (cards ["Plains", "Island", "Forest"])) {Seat.hand = Seq.fromList (cards ["Volunteer Militia"]), Seat.library = Seq.fromList (cards (replicate 5 "Plains"))}
      bob = (S.battlefield S.bob []) {Seat.library = Seq.fromList (cards (replicate 5 "Forest"))}
  gs <- Staged.state <$> S.buildBoardOrFail s registry (S.board (alice NonEmpty.:| [bob]) S.alice S.precombatMain)
  pure gs {GameState.turn = (GameState.turn gs) {TurnRegisters.priority = Just S.alice}}

-- The rest of Alice's turn played on from `gs`'s priority decision
-- (Engine.resumePriority), Alice the search bot at `budget`, Bob the random
-- bot, from `seed`: whether a Volunteer Militia is on the battlefield at
-- the first prompt of the next turn, and the most of Alice's lands tapped at
-- any prompt through it; Nothing past 400 prompts.
alicesTurn :: forall m n. (Monad m) => Spec.Spec m n -> SearchBudget.SearchBudget -> GameState.GameState -> Word.Word32 -> m (Maybe (Bool, Int))
alicesTurn s budget gs seed = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (go (400 :: Int) 0 (State.runStateT Engine.resumePriority gs)) (S.seeded seed))
  where
    registry :: Registry.Registry m
    registry = S.fixtureRegistry []
    seats = Game.consulting registry (Map.fromList [(S.alice, Search.search registry budget Weights.neutral Search.known (pure []) (pure Search.blank) Search.unkept), (S.bob, Game.bot registry)])
    tapped g = length [oid | oid <- Set.toList (GameState.battlefield g), Projection.View.controllerOf oid g == Just S.alice, Set.member CardType.Land (ProjectedCharacteristics.cardTypes (Projection.project oid g)), fmap Object.tapped (Engine.Game.lookupObject oid g) == Just TapState.Tapped]
    militia g = any (Set.member (CardName.MkCardName (Text.pack "Volunteer Militia")) . ProjectedCharacteristics.names . (`Projection.project` g)) (Set.toList (GameState.battlefield g))
    go :: Int -> Int -> Program.Program (Asked.Asked GameState.GameState) a -> Game.Play m (Maybe (Bool, Int))
    go left most program = case program of
      Program.Then asked k
        | TurnRegisters.activePlayer (GameState.turn (Asked.game asked)) /= S.alice -> pure (Just (militia (Asked.game asked), max most (tapped (Asked.game asked))))
        | left > 0 -> Answerer.unwrap seats asked >>= go (left - 1) (max most (tapped (Asked.game asked))) . k
      _ -> pure Nothing

-- Every recipient a line's targets name.
recipientsOf :: [Search.Step] -> Set.Set Recipient.Recipient
recipientsOf = foldMap (\step -> case step of Search.Targeted targets -> Set.unions (Map.elems targets); _ -> Set.empty)

-- A line's modes chosen, numbers of targets announced, and recipients.
answersOf :: [Search.Step] -> ([Seq.Seq ModeIndex.ModeIndex], [Natural], Set.Set Recipient.Recipient)
answersOf line = ([modes | Search.Moded modes <- line], [n | Search.Announced counts <- line, n <- Map.elems counts], recipientsOf line)

-- Alice's answers to her modes, costs, optional additional costs, values of
-- X, announced numbers of targets and first choice of targets in the game played on from `gs`'s priority decision
-- (Engine.resumePriority), through that choice of targets, Alice the search
-- bot at `budget` keeping the line it chose, Bob the random bot, from `seed`;
-- Nothing if no choice of targets of hers is asked within 200 prompts.
linedIn :: forall m n. (Monad m) => Spec.Spec m n -> SearchBudget.SearchBudget -> GameState.GameState -> Word.Word32 -> m (Maybe [Search.Step])
linedIn s budget gs seed = either (Spec.assertFailure s . Text.unpack) pure =<< State.evalStateT (Except.runExceptT (State.evalStateT (go (200 :: Int) [] (State.runStateT Engine.resumePriority gs)) (S.seeded seed))) Nothing
  where
    lifted :: Registry.Registry (State.StateT (Maybe Search.Line) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    searcher = Search.search lifted budget Weights.neutral Search.known (pure []) (pure Search.blank) (Search.MkAim (Game.underneath State.get) (Game.underneath . State.put))
    seats = Game.consulting lifted (Map.fromList [(S.alice, searcher), (S.bob, Game.bot lifted)])
    go :: Int -> [Search.Step] -> Program.Program (Asked.Asked GameState.GameState) a -> Game.Play (State.StateT (Maybe Search.Line) m) (Maybe [Search.Step])
    go left taken program = case program of
      Program.Then asked k
        | left > 0 -> case Asked.prompt asked of
            Prompt.ChooseTargets _ chooser _ _ | chooser == S.alice -> fmap (\targets -> Just (reverse (Search.Targeted targets : taken))) (Answerer.unwrap seats asked)
            Prompt.ChooseModes _ chooser _ _ _ | chooser == S.alice -> Answerer.unwrap seats asked >>= \modes -> go (left - 1) (Search.Moded modes : taken) (k modes)
            Prompt.AnnounceTargets _ chooser _ _ | chooser == S.alice -> Answerer.unwrap seats asked >>= \counts -> go (left - 1) (Search.Announced counts : taken) (k counts)
            Prompt.ChooseCost _ chooser _ _ _ | chooser == S.alice -> Answerer.unwrap seats asked >>= \candidate -> go (left - 1) (Search.Costed candidate : taken) (k candidate)
            Prompt.ChooseKicker _ chooser _ keyword which _ | chooser == S.alice -> Answerer.unwrap seats asked >>= \times -> go (left - 1) (Search.Kicked keyword which times : taken) (k times)
            Prompt.ChooseX _ chooser _ slot _ _ | chooser == S.alice -> Answerer.unwrap seats asked >>= \value -> go (left - 1) (Search.Valued slot value : taken) (k value)
            _ -> Answerer.unwrap seats asked >>= go (left - 1) taken . k
      _ -> pure Nothing

-- The search bot's answer at `budget` and `weights`, from `seed`.
searchedAt :: (Monad m) => Spec.Spec m n -> SearchBudget.SearchBudget -> Weights.Weights -> Asked.Asked GameState.GameState r -> Word.Word32 -> m r
searchedAt s budget weights asked seed = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.search (S.fixtureRegistry []) budget weights Search.known (pure []) (pure Search.blank) Search.unkept) asked) (S.seeded seed))

-- The search bot's answer, at a budget of 2 samples cut off after 40 prompts,
-- the neutral weights, from `seed`.
searched :: (Monad m) => Spec.Spec m n -> Asked.Asked GameState.GameState r -> Word.Word32 -> m r
searched s = searchedMaking s 0

-- 'searched', making a mistake at this percent of its decisions.
searchedMaking :: (Monad m) => Spec.Spec m n -> Natural -> Asked.Asked GameState.GameState r -> Word.Word32 -> m r
searchedMaking s mistakes asked seed =
  let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = mistakes, SearchBudget.method = SearchMethod.Flat}
   in either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.search (S.fixtureRegistry []) budget Weights.neutral Search.known (pure []) (pure Search.blank) Search.unkept) asked) (S.seeded seed))

-- Whether an action plays a land.
isPlay :: Action.Action -> Bool
isPlay action = case action of
  Action.Play {} -> True
  _ -> False

-- Whether an action casts a card with no permanent type (CR 110.4).
notPermanent :: GameState.GameState -> Action.Action -> Bool
notPermanent gs action = case action of
  Action.Cast oid _ _ -> not (any Card.isPermanentType (ProjectedCharacteristics.cardTypes (Projection.project oid gs)))
  _ -> False

-- Whether an action casts a spell.
isCast :: Action.Action -> Bool
isCast action = case action of
  Action.Cast {} -> True
  _ -> False

-- No feature weighted.
zero :: Features.Features
zero = Features.MkFeatures 0 0 0 0 0 0

-- The game with every player but `pid` at 1 life.
othersAtOneLife :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
othersAtOneLife pid gs = gs {GameState.players = Map.mapWithKey (\p player -> if p == pid then player else player {Player.life = 1}) (GameState.players gs)}

-- The game with `pid` at 1 life.
atOneLife :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
atOneLife pid gs = gs {GameState.players = Map.adjust (\player -> player {Player.life = 1}) pid (GameState.players gs)}

-- The declarer and the creatures of the first combat declaration a program
-- asks; Nothing if it asks anything else first.
declaresAgain :: Program.Program (Asked.Asked GameState.GameState) a -> Maybe (Either (PlayerId.PlayerId, [ObjectId.ObjectId]) (PlayerId.PlayerId, [ObjectId.ObjectId], [ObjectId.ObjectId]))
declaresAgain program = case program of
  Program.Then asked _ -> case Asked.prompt asked of
    Prompt.DeclareAttackers _ pid candidates -> Just (Left (pid, candidates))
    Prompt.DeclareBlockers _ pid blockers attackers -> Just (Right (pid, blockers, attackers))
    _ -> Nothing
  Program.Return _ -> Nothing

-- The step and the menu of the first priority decision a program asks of seat
-- 1, once it has not conceded; Nothing if it asks anything else first.
asksAgain :: Program.Program (Asked.Asked GameState.GameState) a -> Maybe (Phase.Phase, [Action.Action])
asksAgain program = case program of
  Program.Then asked k -> case Asked.prompt asked of
    Prompt.Concede pid | pid == S.alice -> asksAgain (k Concession.Continues)
    Prompt.ChooseAction _ pid menu | pid == S.alice -> Just (TurnRegisters.phase (GameState.turn (Asked.game asked)), menu)
    _ -> Nothing
  Program.Return _ -> Nothing

-- A meta of two decks with even shares, 12 Centaur Courser and 12 Grizzly
-- Bears each beside 8 Forest, the bears deck the opponent's in 'capturing';
-- and the Grizzly Bears.
bearsOrCoursers :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Printing.Printing, NonEmpty.NonEmpty (Natural, Deck.Deck))
bearsOrCoursers s registry = do
  bear <- S.printingOf s registry "Grizzly Bears"
  courser <- S.printingOf s registry "Centaur Courser"
  forest <- S.printingOf s registry "Forest"
  let deck creature = Deck.fromCards (Map.fromList [(creature, 12), (forest, 8)])
  pure (bear, (1, deck courser) NonEmpty.:| [(1, deck bear)])

-- The search bot's priors read off a format's pool: the legality table is
-- read from disk.
poolSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
poolSpec s registry = Spec.describe s "Search pool" $ do
  Spec.it s "CR 400.2 parun play's search bot deals an opponent's unseen cards from the game's format's pool, or the pool named, their list not known" $ do
    (gs, _) <- decision s registry
    courser <- S.printingOf s registry "Centaur Courser"
    shahrazad <- S.printingOf s registry "Shahrazad"
    vintage <- either (Spec.assertFailure s . Text.unpack) pure =<< Legality.shipped (Text.pack "vintage")
    let pool = S.fixtureRegistry (fmap Printing.card [courser, shahrazad])
        dealtBy options format = do
          prior <- Command.priorOf pool options format
          traverse (\n -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.sample prior Search.blank viewer gs) (S.seeded n))) [1 .. 8 :: Word.Word32]
        theirs world = printingsOf world (Search.unseen Search.blank viewer gs S.bob)
    casual <- dealtBy PlayOptions.none Nothing
    Foldable.for_ casual $ \world -> do
      Spec.assertEqWith s "the viewer's view" (PlayerView.viewFor viewer world) (PlayerView.viewFor viewer gs)
      Spec.assertEqWith s "the viewer's own unseen cards, as a whole" (List.sort (sources world (Search.unseen Search.blank viewer gs viewer))) (List.sort (sources gs (Search.unseen Search.blank viewer gs viewer)))
    Spec.assertEqWith s "the opponent's unseen cards, no format, from every card" (Set.fromList (foldMap theirs casual)) (Set.fromList [courser, shahrazad])
    underVintage <- dealtBy PlayOptions.none (Just vintage)
    Spec.assertEqWith s "the opponent's unseen cards, under vintage, which bans Shahrazad" (Set.fromList (foldMap theirs underVintage)) (Set.singleton courser)
    oldSchool <- dealtBy PlayOptions.none {PlayOptions.pool = Just (Text.pack "oldschool")} (Just vintage)
    Spec.assertEqWith s "the opponent's unseen cards, old school's pool named, which lacks Centaur Courser" (Set.fromList (foldMap theirs oldSchool)) (Set.singleton shahrazad)

  Spec.it s "CR 108.2a a pool holds no card a library cannot, nontraditional, an Attraction, a Contraption or a conspiracy, and no synthetic card" $ do
    cards <- traverse (fmap Printing.card . S.printingOf s registry) ["Centaur Courser", "Agyrem", "Bumper Cars", "Arms Depot", "Backup Plan", "Synthetic Ability Audit"]
    found <- Command.poolOf (pure (Left (Text.pack "no pool, so no table"))) Nothing (fmap Right cards)
    Spec.assertEqWith s "the pool" (fmap (fmap (Face.name . Card.combined . Printing.card) . Maybe.catMaybes . Foldable.toList) found) (Right [CardName.MkCardName (Text.pack "Centaur Courser")])
  where
    viewer = S.alice

-- Seat 1's first priority decision with a choice from turn 3 on, in
-- 'capturing''s game, at which seat 2 shows cards `shown` takes.
showing :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> (Map.Map Printing.Printing Natural -> Bool) -> m GameState.GameState
showing s registry shown = fmap fst (capturing s registry 5 "a decision" found)
  where
    found :: Asked.Asked GameState.GameState r -> Maybe (GameState.GameState, ())
    found asked = case Asked.prompt asked of
      Prompt.ChooseAction decider pid menu
        | pid == S.alice && Search.searchable asked decider pid (Search.options menu) && shown (Search.seen Search.blank S.alice (Asked.game asked) S.bob) -> Just (Asked.game asked, ())
      _ -> Nothing

-- The printings behind objects.
printingsOf :: GameState.GameState -> [ObjectId.ObjectId] -> [Printing.Printing]
printingsOf gs oids = [printing | oid <- oids, object <- Foldable.toList (Engine.Game.lookupObject oid gs), Source.OfCard pid <- [Object.source object], printing <- Foldable.toList (Engine.Game.printingOf pid gs)]

-- The cards behind objects.
sources :: GameState.GameState -> [ObjectId.ObjectId] -> [Source.Source]
sources gs = foldMap (foldMap (pure . Object.source) . (`Engine.Game.lookupObject` gs))

-- A player's hand, the card behind each object in order.
handOf :: GameState.GameState -> PlayerId.PlayerId -> [Source.Source]
handOf gs pid = sources gs (Foldable.toList (Map.findWithDefault mempty pid (GameState.hand gs)))
