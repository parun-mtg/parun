{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Sacrifice where

import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import qualified Parun.Codec.DifferentIn as DifferentIn
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.ObjectCount as ObjectCount
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Sacrifice as Sacrifice

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464). The keyword codec is a PARAMETER; see
-- Parun.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Sacrifice.Sacrifice keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" ObjectCount.codec Sacrifice.count
  whichPermanents <- Fields.required "whichPermanents" (Filter.codec keywordCodec) Sacrifice.whichPermanents
  -- Defaulted: an absent key is a cost that prints no "with different ...",
  -- which is nearly every card file.
  differentIn <- Fields.defaulted "differentIn" Set.empty (Common.set DifferentIn.codec) Sacrifice.differentIn
  pure Sacrifice.MkSacrifice {Sacrifice.count = count, Sacrifice.whichPermanents = whichPermanents, Sacrifice.differentIn = differentIn}
