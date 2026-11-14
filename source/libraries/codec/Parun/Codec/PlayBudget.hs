{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayBudget where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayAllowance as PlayAllowance
import qualified Parun.Codec.Timestamp as Timestamp
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayBudget as PlayBudget

-- | Every field required: this is a game state's record of a budget already
-- granted, and neither has a default the rules give.
codec :: Codec.Codec PlayBudget.PlayBudget
codec = Fields.object $ do
  remaining <- Fields.required "remaining" PlayAllowance.codec PlayBudget.remaining
  covers <- Fields.required "covers" (Common.set ObjectId.codec) PlayBudget.covers
  granted <- Fields.required "granted" Timestamp.codec PlayBudget.granted
  pure PlayBudget.MkPlayBudget {PlayBudget.remaining = remaining, PlayBudget.covers = covers, PlayBudget.granted = granted}
