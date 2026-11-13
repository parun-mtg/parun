{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PregameExile where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PregameExile as PregameExile

-- | The count defaults to the printed singular, exactly one.
codec :: Codec.Codec PregameExile.PregameExile
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PregameExile.filter
  atLeast <- Fields.defaulted "atLeast" 1 Common.natural PregameExile.atLeast
  atMost <- Fields.defaulted "atMost" (Just 1) (Common.maybe Common.natural) PregameExile.atMost
  pure PregameExile.MkPregameExile {PregameExile.filter = filter_, PregameExile.atLeast = atLeast, PregameExile.atMost = atMost}
