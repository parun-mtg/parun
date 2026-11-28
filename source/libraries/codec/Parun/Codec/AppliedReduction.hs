{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AppliedReduction where

import qualified Data.Text as Text
import qualified Parun.Codec.ManaCost as ManaCost
import qualified Parun.Codec.ManaFilter as ManaFilter
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AppliedReduction as AppliedReduction
import qualified Parun.Types.ManaFilter as ManaFilter.Type

-- | A bare object keyed by the record's field names. @atLeast@ and
-- @reducesOnly@ are DEFAULTED to the rules' own answers -- no floor past CR
-- 601.2f's {0}, and CR 118.7b-d's spill -- so only a card printing Heartstone's
-- or Edgewalker's sentence writes either.
--
-- Unprinted: a confinement naming where mana came from. A reducing symbol is a
-- type and carries no tag, so a Produced confinement is refused, and
-- Parun.Codec.CostChangeSpec's "rejects a Produced confinement" proves it.
codec :: Codec.Codec AppliedReduction.AppliedReduction
codec = Fields.objectWith typesOnly $ do
  amount <- Fields.required "amount" ManaCost.codec AppliedReduction.amount
  atLeast <- Fields.defaulted "atLeast" 0 Common.natural AppliedReduction.atLeast
  reducesOnly <- Fields.defaulted "reducesOnly" Nothing (Common.maybe ManaFilter.codec) AppliedReduction.reducesOnly
  pure AppliedReduction.MkAppliedReduction {AppliedReduction.amount = amount, AppliedReduction.atLeast = atLeast, AppliedReduction.reducesOnly = reducesOnly}

typesOnly :: AppliedReduction.AppliedReduction -> Either Text.Text AppliedReduction.AppliedReduction
typesOnly reduction = case AppliedReduction.reducesOnly reduction of
  Just (ManaFilter.Type.Produced {}) -> Left (Text.pack "AppliedReduction: a confinement over production tags is unprinted and not supported")
  _ -> Right reduction
