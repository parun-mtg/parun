{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CouldProduce where

import qualified Data.Text as Text
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.ManaFilter as ManaFilter
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CouldProduce as CouldProduce
import qualified Parun.Types.ManaFilter as ManaFilter.Type

-- | A bare object keyed by the record's field names, both required.
--
-- Unprinted: "could produce" asking where mana came from. CR 106.7 asks about
-- types, so a Produced filter here is refused, and
-- Parun.Codec.CouldProduceSpec's "rejects a Produced filter" proves it.
codec :: Codec.Codec CouldProduce.CouldProduce
codec = Fields.objectWith typesOnly $ do
  among <- Fields.required "among" (Filter.codec Keyword.codec) CouldProduce.among
  types <- Fields.required "types" ManaFilter.codec CouldProduce.types
  pure CouldProduce.MkCouldProduce {CouldProduce.among = among, CouldProduce.types = types}

typesOnly :: CouldProduce.CouldProduce -> Either Text.Text CouldProduce.CouldProduce
typesOnly could = case CouldProduce.types could of
  ManaFilter.Type.Produced {} -> Left (Text.pack "CouldProduce: a filter over production tags is unprinted and not supported")
  _ -> Right could
