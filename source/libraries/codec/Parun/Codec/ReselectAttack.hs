{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ReselectAttack where

import qualified Parun.Codec.AttackTargetKind as AttackTargetKind
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ReselectAttack as ReselectAttack

-- | "kinds" is required: which of CR 506.3's three things a printing lets the
-- reselection pick is the sentence itself.
codec :: Codec.Codec ReselectAttack.ReselectAttack
codec = Fields.object $ do
  creature <- Fields.required "creature" ObjectRef.codec ReselectAttack.creature
  kinds <- Fields.required "kinds" (Common.set AttackTargetKind.codec) ReselectAttack.kinds
  pure
    ReselectAttack.MkReselectAttack
      { ReselectAttack.creature = creature,
        ReselectAttack.kinds = kinds
      }
