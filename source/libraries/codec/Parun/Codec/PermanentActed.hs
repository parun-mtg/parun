{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PermanentActed where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.PermanentAction as PermanentAction
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PermanentActed as PermanentActed

-- | A bare object keyed by the record's field names, the permanent written with
-- whichever codec its parameter takes and the revealed card with whichever
-- fields the caller gives (`pure ()` writes none).
codec :: (Typeable.Typeable revealed, Typeable.Typeable permanent) => Fields.Fields (PermanentActed.PermanentActed revealed permanent) revealed -> Codec.Codec permanent -> Codec.Codec (PermanentActed.PermanentActed revealed permanent)
codec revealedFields permanent = Fields.object $ do
  action <- Fields.required "action" PermanentAction.codec PermanentActed.action
  actor <- Fields.required "permanent" permanent PermanentActed.permanent
  revealed <- revealedFields
  pure PermanentActed.MkPermanentActed {PermanentActed.action = action, PermanentActed.permanent = actor, PermanentActed.revealed = revealed}
