{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PermanentActsTrigger where

import qualified Parun.Codec.ActingPermanent as ActingPermanent
import qualified Parun.Codec.Batching as Batching
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PermanentAction as PermanentAction
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Batching as Batching.Type
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PermanentActsTrigger as PermanentActsTrigger

-- | A bare object holding the action, the permanent and the revealed card's
-- Filter beside the batching; no Filter and EachOccurrence write no key.
codec :: Codec.Codec PermanentActsTrigger.PermanentActsTrigger
codec = Fields.object $ do
  action <- Fields.required "action" PermanentAction.codec (PermanentActed.action . PermanentActsTrigger.acted)
  actor <- Fields.required "permanent" ActingPermanent.codec (PermanentActed.permanent . PermanentActsTrigger.acted)
  revealed <- Fields.defaulted "revealed" Nothing (Common.maybe (Filter.codec Keyword.codec)) (PermanentActed.revealed . PermanentActsTrigger.acted)
  batching <- Fields.defaulted "batching" Batching.Type.EachOccurrence Batching.codec PermanentActsTrigger.batching
  pure
    PermanentActsTrigger.MkPermanentActsTrigger
      { PermanentActsTrigger.acted = PermanentActed.MkPermanentActed {PermanentActed.action = action, PermanentActed.permanent = actor, PermanentActed.revealed = revealed},
        PermanentActsTrigger.batching = batching
      }
