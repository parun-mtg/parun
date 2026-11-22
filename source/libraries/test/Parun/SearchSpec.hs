{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Covers Parun.Play.Search, Parun.Engine.Engine.resumePriority and
-- Parun.Engine.Engine.resumeTurnBasedAction: a sampled world, a restarted
-- priority decision or combat declaration, and playouts of one decision.
module Parun.SearchSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
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
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Legality as Legality
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Search as Search
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Concession as Concession
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Features as Features
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Program as Program
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.RevealCause as RevealCause
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.Source as Source
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

  Spec.it s "an opponent's pass in their main phase, a land played and two untapped, weighs worlds dealing them a Grizzly Bears to cast less" $ do
    (gs, passes) <- afterPass s registry
    bear <- S.printingOf s registry "Grizzly Bears"
    let played :: Game.Play m a -> Word.Word32 -> m a
        played act n = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT act (S.seeded n))
        heldBears world = toRational (length (filter (== bear) (printingsOf world (filter (`elem` handIds gs S.bob) (Search.unseen Search.blank viewer gs S.bob)))))
        mean weighed = sum [w * heldBears world | (w, world) <- weighed] / sum (fmap fst weighed)
    candidates <- traverse (played (Search.sample Search.known Search.blank viewer gs)) [1 .. 32]
    unweighed <- traverse (\(world, n) -> fmap (\w -> (w, world)) (played (Search.plausibility Search.known Search.blank viewer [] gs world) n)) (zip candidates [1 ..])
    weighed <- traverse (\(world, n) -> fmap (\w -> (w, world)) (played (Search.plausibility Search.known Search.blank viewer passes gs world) n)) (zip candidates [1 ..])
    Spec.assertEqWith s "every weight, no pass watched" (fmap fst unweighed) (1 <$ candidates)
    Spec.assertBool s (mean weighed < mean unweighed) ("the Grizzly Bears held, weighed by the pass (" <> show (fromRational (mean weighed) :: Double) <> ") fewer than not (" <> show (fromRational (mean unweighed) :: Double) <> ")")
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 0, SearchBudget.prompts = 0, SearchBudget.candidates = 32, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0}
        dealt watched = played (Search.worlds budget Search.known Search.blank watched viewer gs >>= Monad.replicateM 200) 1
    inferring <- dealt passes
    blind <- dealt []
    Spec.assertBool s (mean (fmap ((,) 1) inferring) < mean (fmap ((,) 1) blind)) ("the Grizzly Bears held in the worlds dealt, weighing the pass (" <> show (fromRational (mean (fmap ((,) 1) inferring)) :: Double) <> ") fewer than not (" <> show (fromRational (mean (fmap ((,) 1) blind)) :: Double) <> ")")

  Spec.it s "a restarted priority decision asks its holder again, in the same step, offering the same menu" $ do
    (gs, menu) <- decision s registry
    Spec.assertEqWith s "the step and the menu asked again" (asksAgain (State.runStateT Engine.resumePriority gs)) (Just (TurnRegisters.phase (GameState.turn gs), menu))

  Spec.it s "two playouts of one decision share no state" $ do
    (gs, menu) <- decision s registry
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 1000, SearchBudget.prompts = 400, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0}
        playout option n = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget Weights.neutral Engine.resumePriority viewer (Search.acting viewer option) gs) (S.seeded n))
    case Search.options menu of
      one : two : _ -> do
        first <- playout one 3
        _ <- playout two 3
        again <- playout one 3
        Spec.assertEqWith s "the first option's playout again" again first
      _ -> Spec.assertFailure s "a decision offers two options"

  Spec.it s "a playout is cut off once it begins the turn after its decision's, short of a result" $ do
    (gs, menu) <- decision s registry
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 0, SearchBudget.prompts = 100000, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0}
    scores <- traverse (\(option, n) -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget Weights.neutral Engine.resumePriority viewer (Search.acting viewer option) gs) (S.seeded n))) (zip (Search.options menu) [1 ..])
    Spec.assertBool s (all (\score -> abs score < Search.won) scores) ("every playout scored short of a result: " <> show scores)

  Spec.it s "a cut-off playout scores its position by the weights, each side's features weighted" $ do
    (gs, attacker) <- capturing s registry 3 "a declaration of attackers" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareAttackers decider pid candidates | Search.searchable asked decider pid (Search.attacks candidates) -> Just (Asked.game asked, pid)
      _ -> Nothing
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 1000, SearchBudget.prompts = 0, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = 0}
        only side feature = Weights.MkWeights {Weights.ours = if side then feature else zero, Weights.theirs = if side then zero else feature}
        scored weights = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget weights Engine.resumeTurnBasedAction attacker (Search.attacking attacker []) gs) (S.seeded 1))
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

  Spec.it s "the search bot making a mistake at every decision takes the option scored next below the best: no attack on an opponent at 1 life who has nothing untapped" $ do
    (_, asked) <- openAttack s registry
    declared <- traverse (searchedMaking s 100 asked) [1 .. 4]
    Spec.assertBool s (all null declared) ("held back in every seed: " <> show declared)

  Spec.it s "parun play's bot seat is the search bot, attacking an opponent at 1 life who has nothing untapped" $ do
    (pid, asked) <- openAttack s registry
    let seat = PlayerId.unwrap pid + 1
        options = PlayOptions.none {PlayOptions.bots = Set.singleton seat}
        answered seed = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Command.answerer (S.fixtureRegistry []) (const (pure Nothing)) (pure Nothing) (const (pure ())) (pure Map.empty) options Search.known seat) asked) (S.seeded seed))
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

  Spec.it s "a greedy playout plays a land at the decision it restarts" $ do
    (gs, pid) <- capturing s registry 3 "a menu offering a land play" $ \asked -> case Asked.prompt asked of
      Prompt.ChooseAction decider pid menu | any isPlay menu && Search.searchable asked decider pid (Search.options menu) -> Just (Asked.game asked, pid)
      _ -> Nothing
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.turns = 1000, SearchBudget.prompts = 3, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Greedy, SearchBudget.mistakes = 0}
        lands = Weights.MkWeights {Weights.ours = zero {Features.lands = 1}, Weights.theirs = zero}
        before = toInteger (length [oid | oid <- Set.toList (GameState.battlefield gs), Projection.View.controllerOf oid gs == Just pid, Set.member CardType.Land (ProjectedCharacteristics.cardTypes (Projection.project oid gs))])
    scores <- traverse (\n -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget lands Engine.resumePriority pid (Search.MkPick (const Nothing)) gs) (S.seeded n))) [1 .. 8]
    Spec.assertEqWith s "a land more in every seed" scores ((before + 1) <$ scores)
  where
    viewer = S.alice

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
capturingIn s decks seed what found = do
  (_, taken) <- State.runStateT (Game.run lifted Nothing Limits.standard [] (Map.fromList [(Command.idOf 1, capture), (Command.idOf 2, capture)]) (S.seeded seed) Map.empty (Command.seated 1 decks)) Nothing
  maybe (Spec.assertFailure s ("no " <> what <> " from turn 3 on")) pure taken
  where
    lifted :: Registry.Registry (State.StateT (Maybe (GameState.GameState, x)) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    capture :: Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT (Maybe (GameState.GameState, x)) m))
    capture = Answerer.MkAnswerer $ \asked -> case found asked of
      Just taken
        | TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) >= 3 -> do
            Game.underneath (State.put (Just taken))
            Game.stop (Text.pack "found")
      _ -> Answerer.unwrap (Game.bot lifted) asked

-- Seat 1's first priority decision with a choice from turn 3 on, in a game
-- of the bears decks from seed 10 watched by Search.watching, the random bot
-- in both seats, once seat 2 has passed in their own main phase with the
-- stack empty, a land played and two lands untapped, and still holds a card
-- they held then: the game as asked and the passes kept that are such.
afterPass :: forall m n. (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, [Search.Passed])
afterPass s registry = do
  decks <- fmap snd <$> BotSpec.bears s registry
  (_, (_, taken)) <- State.runStateT (Game.run lifted Nothing Limits.standard [Search.watching (\f -> State.modify' (\(passes, taken) -> let kept = f passes in kept `seq` (kept, taken)))] (Map.fromList [(Command.idOf 1, capture), (Command.idOf 2, capture)]) (S.seeded 10) Map.empty (Command.seated 1 decks)) ([], Nothing)
  maybe (Spec.assertFailure s "no decision after an informative pass") pure taken
  where
    lifted :: Registry.Registry (State.StateT ([Search.Passed], Maybe (GameState.GameState, [Search.Passed])) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    informative gs (pid, past) =
      let turn = GameState.turn past
          lands = [oid | oid <- Set.toList (GameState.battlefield past), Projection.View.controllerOf oid past == Just pid, Set.member CardType.Land (ProjectedCharacteristics.cardTypes (Projection.project oid past))]
          untapped = [oid | oid <- lands, fmap Object.tapped (Engine.Game.lookupObject oid past) == Just TapState.Untapped]
       in pid == S.bob
            && TurnRegisters.activePlayer turn == pid
            && elem (TurnRegisters.phase turn) [Phase.PrecombatMain, Phase.PostcombatMain]
            && null (TurnRegisters.stack turn)
            && Map.findWithDefault 0 pid (GameState.landsPlayed past) >= 1
            && length untapped >= 2
            && not (Search.stale gs (pid, past))
    capture :: Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT ([Search.Passed], Maybe (GameState.GameState, [Search.Passed])) m))
    capture = Answerer.MkAnswerer $ \asked -> do
      (passes, _) <- Game.underneath State.get
      case Asked.prompt asked of
        Prompt.ChooseAction decider pid menu
          | pid == S.alice,
            Search.searchable asked decider pid (Search.options menu),
            TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) >= 3,
            any (informative (Asked.game asked)) passes -> do
              Game.underneath (State.put (passes, Just (Asked.game asked, filter (informative (Asked.game asked)) passes)))
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

-- The search bot's answer, at a budget of 2 samples cut off after 40 prompts,
-- the neutral weights, from `seed`.
searched :: (Monad m) => Spec.Spec m n -> Asked.Asked GameState.GameState r -> Word.Word32 -> m r
searched s = searchedMaking s 0

-- 'searched', making a mistake at this percent of its decisions.
searchedMaking :: (Monad m) => Spec.Spec m n -> Natural -> Asked.Asked GameState.GameState r -> Word.Word32 -> m r
searchedMaking s mistakes asked seed =
  let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.turns = 1000, SearchBudget.prompts = 40, SearchBudget.candidates = 0, SearchBudget.playout = Playout.Random, SearchBudget.mistakes = mistakes}
   in either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.search (S.fixtureRegistry []) budget Weights.neutral Search.known (pure []) (pure Search.blank)) asked) (S.seeded seed))

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
