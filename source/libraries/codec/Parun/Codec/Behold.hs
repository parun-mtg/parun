{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Behold where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.SubtypeFamily as SubtypeFamily
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Behold as Behold
import qualified Parun.Types.Zone as ZoneType

-- | A bare object keyed by the record's field names, Parun.Codec.ExileMaterials'
-- shape. The keyword codec is a PARAMETER; see Parun.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Behold.Behold keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural Behold.count
  whichObjects <- Fields.required "whichObjects" (Filter.codec keywordCodec) Behold.whichObjects
  chosenType <- Fields.defaulted "chosenType" Nothing (Common.maybe SubtypeFamily.codec) Behold.chosenType
  cardZone <- Fields.defaulted "cardZone" ZoneType.Hand Zone.codec Behold.cardZone
  pure Behold.MkBehold {Behold.count = count, Behold.whichObjects = whichObjects, Behold.chosenType = chosenType, Behold.cardZone = cardZone}
