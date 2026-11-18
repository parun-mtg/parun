{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChangeTargets where

import qualified Parun.Codec.ChangeCount as ChangeCount
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.RecipientRef as RecipientRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChangeCount as ChangeCount
import qualified Parun.Types.ChangeTargets as ChangeTargets

-- | The stack objects are required; `count` defaults to CR 115.7a's every
-- target, and `onto` to absent, a free change (Deflection).
codec :: Codec.Codec ChangeTargets.ChangeTargets
codec = Fields.object $ do
  objects <- Fields.required "objects" ObjectRef.codec ChangeTargets.objects
  count <- Fields.defaulted "count" ChangeCount.Every ChangeCount.codec ChangeTargets.count
  onto <- Fields.defaulted "onto" Nothing (Common.maybe RecipientRef.codec) ChangeTargets.onto
  pure
    ChangeTargets.MkChangeTargets
      { ChangeTargets.objects = objects,
        ChangeTargets.count = count,
        ChangeTargets.onto = onto
      }
