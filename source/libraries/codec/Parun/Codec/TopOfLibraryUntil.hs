{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TopOfLibraryUntil where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.SideDeck as SideDeck
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.TopOfLibraryUntil as TopOfLibraryUntil

-- | A bare object keyed by the record's field names, the shape every other
-- 'Parun.Types.ObjectRef' payload record takes. All three keys are required: a
-- walk with no filter never stops, one with no library has nothing to walk, and
-- one with no count does not say how many matches end it -- @count@ being
-- 'Parun.Codec.TopOfLibrary''s key, spelled the same way and holding the same
-- 'Parun.Codec.Quantity', so Treasure Hunt's single match writes
-- @{"type":"Literal","value":1}@ where Open the Way's writes an @InSlot@ naming
-- X. @weight@ defaults to a literal 1, a tally of matches; Improvisation
-- Capstone's is the card's mana value.
codec :: Codec.Codec TopOfLibraryUntil.TopOfLibraryUntil
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec TopOfLibraryUntil.player
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) TopOfLibraryUntil.filter
  weight <- Fields.defaulted "weight" (Quantity.Type.Literal 1) Quantity.codec TopOfLibraryUntil.weight
  count <- Fields.required "count" Quantity.codec TopOfLibraryUntil.count
  deck <- Fields.defaulted "deck" Nothing (Common.maybe SideDeck.codec) TopOfLibraryUntil.deck
  pure
    TopOfLibraryUntil.MkTopOfLibraryUntil
      { TopOfLibraryUntil.player = player,
        TopOfLibraryUntil.filter = filter_,
        TopOfLibraryUntil.weight = weight,
        TopOfLibraryUntil.count = count,
        TopOfLibraryUntil.deck = deck
      }
