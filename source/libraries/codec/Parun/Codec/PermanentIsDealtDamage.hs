{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PermanentIsDealtDamage where

import qualified Parun.Codec.ActingPermanent as ActingPermanent
import qualified Parun.Codec.Batching as Batching
import qualified Parun.Codec.DealtDamage as DealtDamage
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Batching as BatchingType
import qualified Parun.Types.PermanentIsDealtDamage as PermanentIsDealtDamage

-- | A bare object keyed by the record's field names. OneOrMore, the printed
-- "is dealt damage", writes no `batching` key.
codec :: Codec.Codec PermanentIsDealtDamage.PermanentIsDealtDamage
codec = Fields.object $ do
  dealt <- Fields.required "dealt" (DealtDamage.codec ActingPermanent.codec) PermanentIsDealtDamage.dealt
  batching <- Fields.defaulted "batching" BatchingType.OneOrMore Batching.codec PermanentIsDealtDamage.batching
  wholeBatch <- Fields.defaulted "wholeBatch" False Common.boolean PermanentIsDealtDamage.wholeBatch
  excess <- Fields.defaulted "excess" False Common.boolean PermanentIsDealtDamage.excess
  atLeast <- Fields.defaulted "atLeast" 0 Common.natural PermanentIsDealtDamage.atLeast
  pure PermanentIsDealtDamage.MkPermanentIsDealtDamage {PermanentIsDealtDamage.dealt = dealt, PermanentIsDealtDamage.batching = batching, PermanentIsDealtDamage.wholeBatch = wholeBatch, PermanentIsDealtDamage.excess = excess, PermanentIsDealtDamage.atLeast = atLeast}
