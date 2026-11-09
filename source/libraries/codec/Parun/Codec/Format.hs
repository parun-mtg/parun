{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Format where

import qualified Data.Set as Set
import qualified Parun.Codec.DraftBooster as DraftBooster
import qualified Parun.Codec.RuleFamily as RuleFamily
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Format as Format

-- | A bare object keyed by the record's field names, a file under
-- data/formats.
codec :: Codec.Codec Format.Format
codec = Fields.object $ do
  rules <- Fields.required "rules" RuleFamily.codec Format.rules
  pool <- Fields.defaulted "pool" Nothing (Common.maybe Common.text) Format.pool
  boosters <- Fields.defaulted "boosters" Set.empty (Common.set DraftBooster.codec) Format.boosters
  pure Format.MkFormat {Format.rules = rules, Format.pool = pool, Format.boosters = boosters}
