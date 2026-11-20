module Parun.Engine.LinkReaders where

import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Interchangeable.Mentions as Mentions
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Types.DelayedTrigger as DelayedTrigger
import qualified Parun.Types.EntryLink as EntryLink
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.LastKnown as LastKnown
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.StoredEffects as StoredEffects

-- | CR 607.1 / 607.5 / 608.2h: can `source`, live or gone, read what its own
-- effects linked to objects -- the counters they moved
-- (Filter.CounterLinkedToSource) or the permanents they put onto the
-- battlefield (Filter.EnteredWithSource)? Asked of the cards representing it,
-- its projection (CR 613.1: a copy effect, a full text, a grant), else its
-- last known characteristics and copiable values, and the delayed triggers it
-- made (CR 603.7). Erring towards yes, the safe direction.
readsOwnLinks :: GameState -> ObjectId -> Bool
readsOwnLinks gs source =
  let projected = Monoid.getAny . Mentions.projectedCharacteristicsNames Mentions.readsSourceLinks
   in any (Monoid.getAny . Mentions.cardNames Mentions.readsSourceLinks) (Game.cardsOfWithLastKnown source gs)
        || or (Game.liveOrLastKnown (\_ -> projected (Projection.project source gs)) (\known -> projected (LastKnown.characteristics known) || projected (LastKnown.copiable known)) source gs)
        || any (\entry -> DelayedTrigger.source entry == source && Monoid.getAny (Mentions.delayedTriggerNames Mentions.readsSourceLinks entry)) (StoredEffects.delayedTriggers (GameState.stored gs))

-- | CR 607.5: link `entrant` to the source `link` names, for
-- Filter.EnteredWithSource. Adds to the permanent's links rather than
-- replacing them, and writes nothing where the source cannot read it
-- (readsOwnLinks), so a row no reader reaches never parts two permanents
-- (Parun.Engine.Interchangeable.namedByRelation). Parun.ManaSourceSpec's
-- "CR 610.3 an Elf Banisher Priest returned is no candidate of its own"
-- proves it.
recordEntry :: EntryLink.EntryLink -> ObjectId -> GameState -> GameState
recordEntry link entrant gs
  | readsOwnLinks gs (EntryLink.source link) = gs {GameState.enteredWith = Map.insertWith Set.union entrant (Set.singleton link) (GameState.enteredWith gs)}
  | otherwise = gs
