{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayerScope where

import Data.Set (Set)
import qualified Parun.Codec.AttackTargetKind as AttackTargetKind
import qualified Parun.Codec.AttackedDuring as AttackedDuring
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.Superlative as Superlative
import qualified Parun.Codec.TurnRole as TurnRole
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.AttackedDuring as AttackedDuring
import qualified Parun.Types.PlayerScope as PlayerScope

codec :: Codec.Codec PlayerScope.PlayerScope
codec =
  Arm.tagged
    tagOf
    ( PlayerRelation.arms PlayerScope.Related
        <> [ Arm.payload "Superlative" Superlative.codec PlayerScope.Superlative (\x -> case x of PlayerScope.Superlative y -> Just y; _ -> Nothing),
             Arm.payload "InRole" TurnRole.codec PlayerScope.InRole (\x -> case x of PlayerScope.InRole y -> Just y; _ -> Nothing),
             Arm.nullary "ChosenPlayer" PlayerScope.ChosenPlayer,
             Arm.nullary "SourceOwner" PlayerScope.SourceOwner,
             Arm.nullary "EnchantedPlayer" PlayerScope.EnchantedPlayer,
             Arm.payload "AttackedYou" attackedYou (uncurry PlayerScope.AttackedYou) (\x -> case x of PlayerScope.AttackedYou during kinds -> Just (during, kinds); _ -> Nothing)
           ]
    )

tagOf :: PlayerScope.PlayerScope -> String
tagOf x = case x of
  PlayerScope.Related relation -> PlayerRelation.tag relation
  PlayerScope.Superlative {} -> "Superlative"
  PlayerScope.InRole {} -> "InRole"
  PlayerScope.ChosenPlayer {} -> "ChosenPlayer"
  PlayerScope.SourceOwner {} -> "SourceOwner"
  PlayerScope.EnchantedPlayer {} -> "EnchantedPlayer"
  PlayerScope.AttackedYou {} -> "AttackedYou"

-- | PlayerScope.AttackedYou's two keys, "kinds" required for Parun.Codec.AimedAt's reason.
attackedYou :: Codec.Codec (AttackedDuring.AttackedDuring, Set AttackTargetKind.AttackTargetKind)
attackedYou = Fields.object $ do
  during <- Fields.required "during" AttackedDuring.codec fst
  kinds <- Fields.required "kinds" (Common.set AttackTargetKind.codec) snd
  pure (during, kinds)
