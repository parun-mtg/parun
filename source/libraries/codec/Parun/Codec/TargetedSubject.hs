{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TargetedSubject where

import qualified Parun.Codec.ActingPermanent as ActingPermanent
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ActingPermanent as ActingPermanentType
import qualified Parun.Types.PlayerRelation as PlayerRelationType
import qualified Parun.Types.TargetedSubject as TargetedSubject

codec :: Codec.Codec TargetedSubject.TargetedSubject
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Permanent" ActingPermanent.codec TargetedSubject.Permanent (\x -> case x of TargetedSubject.Permanent y -> Just y; _ -> Nothing),
      Arm.payload "Player" PlayerRelation.codec TargetedSubject.Player (\x -> case x of TargetedSubject.Player y -> Just y; _ -> Nothing),
      Arm.payload "PlayerOrPermanent" playerOrPermanent (uncurry TargetedSubject.PlayerOrPermanent) (\x -> case x of TargetedSubject.PlayerOrPermanent r p -> Just (r, p); _ -> Nothing)
    ]

-- | PlayerOrPermanent's two fields, keyed.
playerOrPermanent :: Codec.Codec (PlayerRelationType.PlayerRelation, ActingPermanentType.ActingPermanent)
playerOrPermanent = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec fst
  permanent <- Fields.required "permanent" ActingPermanent.codec snd
  pure (player, permanent)

tagOf :: TargetedSubject.TargetedSubject -> String
tagOf x = case x of
  TargetedSubject.Permanent {} -> "Permanent"
  TargetedSubject.Player {} -> "Player"
  TargetedSubject.PlayerOrPermanent {} -> "PlayerOrPermanent"
