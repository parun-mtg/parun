{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Parun.Engine.Dungeon (CR 309 and CR 701.49) where a scenario cannot
-- reach it: a turn handed to another player mid-resolution, and the deck's road
-- into Player.dungeons. The rest of venturing is in data/scenarios/dungeon.
module Parun.DungeonSpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Card as Card
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.Player as Player
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

-- Secret Door's one activated ability, "{4}{U}: Venture into the dungeon.
-- Activate only as a sorcery." Taken off the printing rather than rebuilt, so the
-- case exercises the card file.
ventureAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)
ventureAbility printing = case concatMap Face.activatedAbilities (Card.faces (Printing.card printing)) of
  ab : _ -> ab
  [] -> error "Secret Door has no activated ability"

-- Answers every prompt an activation and a room ability raise: pay mana with
-- whatever is offered first, take every target offered, and otherwise defer to
-- S.identityAnswer -- which answers Prompt.ChooseRoom with the FIRST arrow.
paying :: Prompt.Prompt r -> r
paying p = case p of
  Prompt.ChooseManaSource _ _ _ candidates _ -> Just (NonEmpty.head candidates)
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(n, cands) -> Set.fromList (take (Natural.toIntSaturating n) (Set.toList cands))) sets
  _ -> S.identityAnswer p

-- Resolve the whole stack, settling between each resolution so a room ability that
-- triggered on the venture is placed and then resolved too. Bounded, so a bug that
-- kept the stack full fails the case rather than hanging it.
resolveAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAll answer =
  let go n gs
        | n <= 0 = gs
        | null (TurnRegisters.stack (GameState.turn gs)) = gs
        | otherwise = go (n - 1) (S.runPure answer gs (Stack.resolveTop >> Engine.settleForPriority))
   in go (20 :: Int)

-- alice controls a Secret Door and `lands` untapped Islands, owns `dungeons`
-- outside the game (CR 309.2), and has cards left to draw.
--
-- The lands are what make repeated ventures possible in ONE main phase: Secret
-- Door's ability has no tap in its cost, so the only limit on how deep a case can
-- go is mana.
dungeonBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Int -> (ObjectId, GameState.GameState)
dungeonBoard island door dungeons lands =
  let stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) (S.landsInPlay island lands) [1 .. (4 :: Int)]
      (doorId, g1) = S.addPermanent door S.alice stocked
      -- CR 309.2: a dungeon is recorded on the player and no object is minted for
      -- it, so its printing is interned here rather than by an object build.
      intern (ids, g) printing = let (i, g3) = Game.intern printing g in (ids <> [i], g3)
      (dungeonIds, g2) = List.foldl' intern ([], g1) dungeons
      owned p = p {Player.dungeons = Set.fromList dungeonIds}
   in (doorId, g2 {GameState.players = Map.adjust owned S.alice (GameState.players g2)})

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Engine.Dungeon" $ do
  -- CR 309.4c: "each room ability is controlled by the player who owns the
  -- dungeon card", whoever's turn it is. alice's venture resolves on bob's turn,
  -- so a room ability handed to the active player would scry bob's library, or
  -- -- not being moved by its controller -- not trigger at all.
  Spec.it s "CR 309.4c a room ability is the dungeon owner's on another player's turn" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    door <- S.printingOf s registry "Secret Door"
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    let (doorId, base) = dungeonBoard island door [lostMine] 5
        (marker, gs) = S.addLibraryCard mountain S.alice base
        answering :: Prompt.Prompt r -> r
        answering p = case p of
          Prompt.ChooseScry {} -> ([marker], [])
          _ -> paying p
        activated = S.runPure answering gs (Activate.activateAbility S.alice doorId (ventureAbility door))
        onBobsTurn =
          activated
            { GameState.turn =
                (GameState.turn activated)
                  { TurnRegisters.activePlayer = S.bob
                  }
            }
        resolved = resolveAll answering (S.runPure answering onBobsTurn Engine.settleForPriority)
        libraryOf = Game.zoneMembers Zone.Library S.alice
        -- By name: a card scried is put back as a new object (CR 701.20e).
        bottomName = fmap (\oid -> fmap S.nameOf (Game.cardOf oid resolved)) (take 1 (reverse (libraryOf resolved)))
    Spec.assertEqWith s "bob is the active player" (TurnRegisters.activePlayer (GameState.turn resolved)) S.bob
    Spec.assertEqWith s "alice scried her Mountain to the bottom" bottomName [Just (S.printingName mountain)]

  -- CR 309.2: the dungeons a player owns come from their DECK definition, the
  -- path the board above skips by writing Player.dungeons directly.
  Spec.it s "CR 309.2 a deck's dungeon cards are recorded on its player and minted into no zone" $ do
    lostMine <- S.printingOf s registry "Lost Mine of Phandelver"
    tomb <- S.printingOf s registry "Tomb of Annihilation"
    let deck = Deck.MkDeck {Deck.cards = Map.empty, Deck.commander = Map.empty, Deck.vanguard = Nothing, Deck.dungeons = Set.fromList [lostMine, tomb], Deck.sideboard = Map.empty, Deck.conspiracies = Map.empty, Deck.sideDecks = Map.empty, Deck.stickerSheets = Seq.empty, Deck.draftNotes = Map.empty, Deck.exiledBeforeGame = Map.empty}
        after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Setup.createDeck S.alice deck)
        ownedBy pid = fmap List.sort (traverse (\i -> Game.printingOf i after) (Set.toList (maybe Set.empty Player.dungeons (Map.lookup pid (GameState.players after)))))
    Spec.assertEqWith s "alice owns both" (ownedBy S.alice) (Just (List.sort [lostMine, tomb]))
    Spec.assertEqWith s "bob owns none" (fmap Player.dungeons (Map.lookup S.bob (GameState.players after))) (Just Set.empty)
    -- CR 309.2 / 400.11: it begins OUTSIDE the game, so setup mints no object for
    -- it at all.
    Spec.assertEqWith s "and no object was minted for it" (Map.size (GameState.objects after)) 0
