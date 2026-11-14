{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.JoinAttack where

import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.JoinAttack as JoinAttack

codec :: Codec.Codec JoinAttack.JoinAttack
codec = Fields.object $ do
  creature <- Fields.required "creature" ObjectRef.codec JoinAttack.creature
  alongside <- Fields.required "alongside" PlayerRef.codec JoinAttack.alongside
  pure
    JoinAttack.MkJoinAttack
      { JoinAttack.creature = creature,
        JoinAttack.alongside = alongside
      }
