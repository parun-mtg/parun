{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Meld where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.EntryRiders as EntryRiders
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.EntryRiders as EntryRiders
import qualified Parun.Types.Meld as Meld

-- | `objects` and `result` required: CR 701.42a names the cards and the
-- combined back face, and neither has a default a card file could elide.
-- `riders` is elided at CR 110.5b's default, Parun.Codec.Create's posture.
--
-- The card codec is a PARAMETER, Parun.Codec.Create's posture: this arm is where
-- card data nests inside card data, and Parun.Codec.Card is what ties the knot.
codec :: (Typeable.Typeable card, Typeable.Typeable ability, Eq ability) => Codec.Codec card -> Codec.Codec ability -> Codec.Codec (Meld.Meld card ability)
codec cardCodec abilityCodec = Fields.object $ do
  objects <- Fields.required "objects" ObjectRef.codec Meld.objects
  result <- Fields.required "result" cardCodec Meld.result
  riders <- Fields.defaulted "riders" EntryRiders.defaultValue (EntryRiders.codec abilityCodec) Meld.riders
  pure
    Meld.MkMeld
      { Meld.objects = objects,
        Meld.result = result,
        Meld.riders = riders
      }
