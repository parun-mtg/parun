{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.GrantCopyCast where

import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.Duration as Duration
import qualified Parun.Codec.ManaSpending as ManaSpending
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.GrantCopyCast as GrantCopyCast
import qualified Parun.Types.ManaSpending as ManaSpending.Type
import qualified Parun.Types.PlayerRef as PlayerRef.Type
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | @duration@ is REQUIRED; @player@ is elided at CR 109.5's "you", @spending@
-- at the printed colours, and @recopy@ where the permission copies nothing.
codec :: Codec.Codec GrantCopyCast.GrantCopyCast
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec GrantCopyCast.duration
  player <- Fields.defaulted "player" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec GrantCopyCast.player
  spending <- Fields.defaulted "spending" ManaSpending.Type.AsProduced ManaSpending.codec GrantCopyCast.spending
  recopy <- Fields.defaulted "recopy" Nothing (Common.maybe Condition.codec) GrantCopyCast.recopy
  pure
    GrantCopyCast.MkGrantCopyCast
      { GrantCopyCast.duration = duration,
        GrantCopyCast.player = player,
        GrantCopyCast.spending = spending,
        GrantCopyCast.recopy = recopy
      }
