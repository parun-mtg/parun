module Parun.Types.EventShape where

import qualified Parun.Types.CardArrivedIn as CardArrivedIn
import qualified Parun.Types.CastWindow as CastWindow
import qualified Parun.Types.MovedBetween as MovedBetween
import qualified Parun.Types.Zone as Zone

-- | Which recorded events a history count folds over. TurnRegisters.events is cleared
-- at the turn change (Parun.Engine.Engine), an engine choice made under CR 608.2i,
-- so the log's extent IS the window -- "this turn" -- for every shape but
-- SpellCast, which names its own (Parun.Types.CastWindow).
--
-- Only the zone changes, GameEvent.LeftTheGame, the cast, the attacker
-- declaration, the counter placement, the sacrifice and combat damage to a
-- player have a shape: every other GameEvent constructor is recorded in the log with no EventShape arm, so a
-- count cannot fold over any of them (#162).
--
-- That is not the same as the log being unreadable for those events, and the
-- readers that exist do not come through here. A fold is the wrong instrument
-- whenever the question is about a PLAYER or about ONE named object rather than
-- about a population of objects, since a member of this fold is a Filter view of
-- an object and the unit counted is an EVENT. Those questions are
-- Parun.Types.Quantity arms instead -- CardsDiscardedThisTurn,
-- PlayersDealtDamage, EnteredThisTurn, EnteredFrom and WasCastFrom among
-- them -- each of which reads TurnRegisters.events directly through a
-- Parun.Engine.Game accessor.
data EventShape
  = -- | CR 700.4: "dies" is MovedBetween Battlefield Graveyard.
    MovedBetween MovedBetween.MovedBetween
  | -- | CR 603.6c: an object left this zone, for anywhere -- "left the
    -- battlefield", which GameEvent.LeftTheGame answers too.
    MovedFrom Zone.Zone
  | -- | CR 713.8e's second half: how many CARDS were put into this zone, from
    -- anywhere.
    --
    -- MovedBetween above is the same rule's first half and counts OBJECTS, which
    -- is why the two cannot be one arm: a melded permanent's death is one object
    -- that moved and two cards that were put into a graveyard, so folding the
    -- arrivals under that shape would make Khabal Ghoul's "each creature that
    -- died this turn" see three.
    --
    -- ONE DESTINATION, where MovedBetween names both ends, and beside it the
    -- origins that do NOT count rather than the ones that do -- see the payload.
    -- CR 713.8e's "changed zones" puts no condition on where a card came from,
    -- and most of the pool's producers ask the same way -- "put into graveyards
    -- from anywhere", and Ashiok, Wicked Manipulator's Nightmare token's "if a
    -- card was put into exile this turn" (Parun.PlaneswalkerSpec's AshiokLoyalty
    -- group is what proves an empty exclusion does not read the origin: it exiles
    -- out of a HAND). CR 903.9c is what makes the destination the load-bearing
    -- half: a melded commander's two cards can land in two different zones, and
    -- only the one that arrived in the named zone is counted.
    --
    -- Into the battlefield it counts every permanent that entered, a created
    -- token among them (CR 111.2), as it entered (CR 608.2i), under the
    -- controller it entered under:
    -- Iron Man, Master of Machines' "if an artifact entered the battlefield
    -- under your control this turn".
    CardArrivedIn CardArrivedIn.CardArrivedIn
  | -- | CR 601.2i: a spell became cast. What "for each spell you've cast this
    -- turn" folds over (Aetherflux Reservoir).
    --
    -- ONLY A WINDOW, unlike MovedBetween above, and that asymmetry is the point: a
    -- zone change is only a shape once its two zones are named, and no Filter atom
    -- can name a zone. Everything else a cast count narrows by IS a Filter atom --
    -- "you've cast" is Filter.HeldBy Holder.Controller against the caster CR 601.2a made the
    -- spell's controller, "instant and sorcery spell" a disjunction of
    -- Filter.HasCardType -- so putting any of it here would be a second way to
    -- say what the Count's own filter already says.
    SpellCast CastWindow.CastWindow
  | -- | CR 508.1a / 608.2i: a creature was declared as an attacker -- "you
    -- attacked with a Hero this turn" (Avengers Assemble!). Each member is the
    -- attacker as it was declared, controlled by the player who attacked with
    -- it. A creature put onto the battlefield attacking never "attacked"
    -- (CR 508.4).
    AttackerDeclared
  | -- | CR 122.6 / 608.2i: counters of one kind were put on an object, each
    -- member the object as they landed -- Lasting Tarfire's "you put a counter
    -- on a creature this turn". Filter.CountersPutThisTurn asks a member that
    -- one placement's putter and kind.
    CountersPut
  | -- | CR 701.21a / 608.2i: a permanent was sacrificed, each member the
    -- permanent as it last existed, its controller the player who sacrificed
    -- it (Dargo, the Shipwrecker).
    Sacrificed
  | -- | CR 510.2 / 608.2i: a source dealt combat damage to a player, each member
    -- the source as it dealt it (Parun.Engine.Filter.dealtByView), and each
    -- source a member once however often it connected (Indulge // Excess).
    DealtCombatDamageToPlayer
  deriving (Eq, Ord, Show)
