-- CR 607.2d / 109.5: the Filter.Context a source's OWN ability is matched in,
-- with every answer the board gives about that source filled in one place --
-- its host (CR 303.4b), its owner (CR 108.3), what it put onto the battlefield
-- (CR 400.7), what its effects moved counters on (CR 607.1), what it exiled
-- (CR 607.2a) and every choice it made as it entered (CR 614.1c) or while
-- resolving (CR 608.2d) -- so no position can fill one and forget another.
--
-- Not the source's power, toughness, mana value, colours or names: those are
-- projected characteristics, and this module sits below Parun.Engine.Projection,
-- whose layer fold (CR 613) builds its own contexts here.
-- Parun.Engine.Projection.sourceContext, framedBy and framedWith add them, and
-- every caller outside the fold goes through those instead.
module Parun.Engine.SourceContext where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Count as Count
import qualified Parun.Engine.Defender as Defender
import qualified Parun.Engine.Filter as Filter
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Plane as Plane
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.Chosen as Chosen
import qualified Parun.Types.CounterLink as CounterLink
import qualified Parun.Types.EntryLink as EntryLink
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.SourceChoices as SourceChoices
import qualified Parun.Types.SourcePlayer as SourcePlayer

-- | Filter.contextFor framed by `source`, with every source-derived field
-- filled (framedBy), objects read through `viewOf`.
sourceContext :: Count.ViewOf -> GameState.GameState -> Maybe PlayerId.PlayerId -> ObjectId.ObjectId -> Filter.Context
sourceContext viewOf gs perspective source =
  framedBy viewOf source gs (Filter.contextIn gs perspective (Just source))

-- | `context` with every source-derived field of `source` filled, its choices
-- read live through CR 608.2h's last known information (choicesOf), for the
-- abilities it has now (Game.acquisitionOf). Leaves
-- `Filter.source` alone, so a caller whose IsSource names nothing keeps that.
framedBy :: Count.ViewOf -> ObjectId.ObjectId -> GameState.GameState -> Filter.Context -> Filter.Context
framedBy viewOf source gs = framedWith viewOf (choicesOf (Game.acquisitionOf source gs) source gs) source gs

-- | framedBy with the choices supplied: what a stored effect baked as it began
-- (CR 608.2h), in place of the source's current ones. Every other field is
-- still read off the board, the entrants under the choices' acquisition. All
-- are thunks: a filter that names none of their atoms forces none of them.
-- `viewOf` is the caller's: the layer fold's bounded view inside CR 613,
-- Projection's full one outside it.
framedWith :: Count.ViewOf -> SourceChoices.SourceChoices -> ObjectId.ObjectId -> GameState.GameState -> Filter.Context -> Filter.Context
framedWith viewOf choices source gs context =
  (withChoices choices context)
    { -- CR 901.12c: a planar source's "you" is the planar team.
      Filter.youIsTeam = Plane.readsYouAsTeam source gs,
      -- CR 608.2h / 113.7a: a source that has left still names the host it
      -- had as it left.
      Filter.sourceAttachedTo = Game.hostWithLastKnown source gs,
      -- CR 607.2f: a word's choosers under the choices' acquisition (CR 607.5).
      Filter.sourcePlayers =
        let acquisition = SourceChoices.acquisition choices
            positions = foldMap (Set.fromList . Map.elems) (Game.choiceWithLastKnown Chosen.NumberBy acquisition source gs)
            named = [SourcePlayer.Enchanted, SourcePlayer.Chosen] <> fmap SourcePlayer.LastChoseWord (Set.toList positions)
         in Map.filter (not . Set.null) (Map.fromList (fmap (\which -> (which, Game.sourcePlayersWithLastKnown acquisition which source gs)) named)),
      -- CR 108.3 fixes an owner when the game starts, so a live read; a source
      -- the state no longer holds answers Nothing.
      Filter.sourceOwner = fmap Object.owner (Game.lookupObject source gs),
      -- CR 607.5: only those the abilities doing the reading put there, and
      -- for a delayed ability only those the resolution that created it put
      -- there (CR 603.7, 608.2c: "this way").
      Filter.sourceEntrants = Map.keysSet (Map.filter (any (readsEntrant choices source)) (GameState.enteredWith gs)),
      -- CR 607.5: the same acquisition's counter links.
      Filter.sourceCountered = Map.filter (not . Set.null) (fmap (Set.map CounterLink.moved . Set.filter (\link -> CounterLink.source link == source && CounterLink.acquisition link == SourceChoices.acquisition choices)) (GameState.counteredWith gs)),
      -- CR 607.2a / 607.1d: the cards in exile now (CR 400.7) linked to the
      -- source.
      Filter.sourceExiled = Map.mapMaybe (Binding.linkAsSeenBy source) (Map.restrictKeys (GameState.exiledWith gs) (GameState.exile gs)),
      -- CR 406.1: every card in exile, for a group anchor over that pool,
      -- whose nest reads sourceExiled above.
      Filter.exiledViews = Seq.fromList (Maybe.mapMaybe viewOf (Foldable.toList (GameState.exile gs))),
      -- CR 401.2: the top card of every library, for a group anchor over that
      -- pool (Skybreen); a top CR 401.5 holds back is none, per the
      -- docs/decisions.md entry "Whether a library's held-back top card is its
      -- top card".
      Filter.libraryTopViews = Seq.fromList (Maybe.mapMaybe (\pid -> Game.shownTop pid gs >>= viewOf) (Map.keys (GameState.players gs))),
      -- CR 607.2d: the objects the source last chose under the choices'
      -- acquisition, read off the board as sourceExiled is.
      Filter.sourceChosenObjects = Maybe.fromMaybe Set.empty (Game.choiceWithLastKnown Chosen.Objects (SourceChoices.acquisition choices) source gs),
      -- CR 508.5, for a source that is attacking or was removed from combat,
      -- and CR 506.4e's protector for a planeswalker that became only a
      -- battle, whose card types `viewOf` answers. data/scenarios/
      -- planeswalker-combat's "swathcutter-giant" scenario proves it for an
      -- ability's own resolution, and its "gaeas-liege" one inside the layer
      -- fold.
      Filter.defendingPlayers = Maybe.maybeToList (Defender.defendingPlayerOf (const viewOf) source gs)
    }

-- | The choices `source`'s abilities acquired as `acquisition` have made so
-- far (CR 607.5), carrying that acquisition, through CR 608.2h's last known
-- information; what a stored effect bakes as it begins.
choicesOf :: Acquisition.Acquisition -> ObjectId.ObjectId -> GameState.GameState -> SourceChoices.SourceChoices
choicesOf acquisition source gs =
  SourceChoices.MkSourceChoices
    { SourceChoices.names = Maybe.fromMaybe Map.empty (Game.choiceWithLastKnown Chosen.Names acquisition source gs),
      SourceChoices.colors = Maybe.fromMaybe Set.empty (Game.choiceWithLastKnown Chosen.Colors acquisition source gs),
      SourceChoices.subtype = Game.choiceWithLastKnown Chosen.Subtype acquisition source gs,
      SourceChoices.cardType = Game.choiceWithLastKnown Chosen.CardType acquisition source gs,
      SourceChoices.number = Game.choiceWithLastKnown Chosen.Number acquisition source gs,
      SourceChoices.acquisition = acquisition,
      SourceChoices.resolution = Nothing
    }

-- | `context` with these choices as the source's: CR 105.2's colours, CR
-- 205.3's subtype, CR 201.4's names and CR 607.2d's number, and the
-- acquisition they belong to (CR 607.5).
withChoices :: SourceChoices.SourceChoices -> Filter.Context -> Filter.Context
withChoices choices context =
  context
    { Filter.sourceChosenColors = SourceChoices.colors choices,
      Filter.sourceChosenSubtype = SourceChoices.subtype choices,
      Filter.sourceChosenCardType = SourceChoices.cardType choices,
      Filter.sourceChosenNumber = SourceChoices.number choices,
      Filter.sourceChosenNames = SourceChoices.names choices,
      Filter.sourceAcquisition = Just (SourceChoices.acquisition choices)
    }

-- | CR 607.5 / 603.7: whether a reader framed by `choices` of `source` reads
-- this entry link: the same source and acquisition, and the resolution that
-- made it where the reader names one.
readsEntrant :: SourceChoices.SourceChoices -> ObjectId.ObjectId -> EntryLink.EntryLink -> Bool
readsEntrant choices source link =
  EntryLink.source link == source
    && EntryLink.acquisition link == SourceChoices.acquisition choices
    && all (== EntryLink.resolution link) (SourceChoices.resolution choices)
