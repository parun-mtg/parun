module Parun.Types.ZoneChangeR where

import qualified Data.Sequence as Seq
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.WithCounters as WithCounters
import qualified Parun.Types.Zone as Zone
import qualified Parun.Types.ZoneChangePattern as ZoneChangePattern

-- | The payload of Parun.Types.ReplacementEffect's ZoneChangeR arm (#1305): the
-- zone change intercepted, the zone the object goes to instead, and the
-- actions the redirect may perform alongside the move.
--
-- `revealing` and `shuffling` default to False and are elided rather than
-- written, DamageR's `riders` posture: a redirect that only names a
-- destination is the common shape. `position` defaults to
-- LibraryPosition.defaultValue, `optional` to False, `counters` to Nothing and
-- `riders` to empty the same way.
--
-- Parametric in the EFFECT for `whenYouDo` and `riders`, Parun.Types.DamageR's
-- reason.
data ZoneChangeR effect = MkZoneChangeR
  { matching :: ZoneChangePattern.ZoneChangePattern,
    destination :: Zone.Zone,
    -- | CR 701.20a: show the moving card to all players, as Nexus of Fate's
    -- "reveal Nexus of Fate and shuffle it into its owner's library instead"
    -- does.
    revealing :: Bool,
    -- | CR 701.24a: randomize the moving card's owner's library, which is what
    -- separates "shuffle it into its owner's library" from putting it on the
    -- bottom.
    shuffling :: Bool,
    -- | CR 401.2: the end of the library a redirect into one puts the card on --
    -- Library of Leng's "on top of your library". Ignored for any other
    -- destination.
    position :: LibraryPosition.LibraryPosition,
    -- | CR 614.1a's "you MAY ... instead": the row's controller is asked as it
    -- applies, and declining leaves the event standing.
    optional :: Bool,
    -- | CR 122.1 / 614.1a: the counters the object arrives with, Goliath
    -- Daydreamer's "exile that card with a dream counter on it instead".
    -- Dropped when a later row moves the destination again.
    counters :: Maybe WithCounters.WithCounters,
    -- | CR 603.11: the linked ability "When you do, ..." triggers each time
    -- the redirect applies (Head of the Hunt). Empty, and elided, for a
    -- redirect linking none.
    whenYouDo :: Seq.Seq effect,
    -- | CR 614.1a / 614.6: what the modified event does beside the move --
    -- Kalitas, Traitor of Ghet's "instead exile that card and create a 2/2
    -- black Zombie creature token". The moved object is bound under
    -- Parun.Engine.Binding.departedPermanent as it last existed (CR 608.2h).
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
