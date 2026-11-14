{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PermanentActsTrigger where

import qualified Parun.Codec.ActingPermanent as ActingPermanent
import qualified Parun.Codec.Batching as Batching
import qualified Parun.Codec.PermanentAction as PermanentAction
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Batching as Batching.Type
import qualified Parun.Types.PermanentActed as PermanentActed
import qualified Parun.Types.PermanentActsTrigger as PermanentActsTrigger

-- | A bare object holding the action and the permanent beside the batching;
-- EachOccurrence writes no key.
codec :: Codec.Codec PermanentActsTrigger.PermanentActsTrigger
codec = Fields.object $ do
  action <- Fields.required "action" PermanentAction.codec (PermanentActed.action . PermanentActsTrigger.acted)
  actor <- Fields.required "permanent" ActingPermanent.codec (PermanentActed.permanent . PermanentActsTrigger.acted)
  batching <- Fields.defaulted "batching" Batching.Type.EachOccurrence Batching.codec PermanentActsTrigger.batching
  pure
    PermanentActsTrigger.MkPermanentActsTrigger
      { PermanentActsTrigger.acted = PermanentActed.MkPermanentActed {PermanentActed.action = action, PermanentActed.permanent = actor},
        PermanentActsTrigger.batching = batching
      }
