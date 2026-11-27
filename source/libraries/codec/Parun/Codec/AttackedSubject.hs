{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AttackedSubject where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.AttackTargetKind as AttackTargetKind
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.SourcePlayer as SourcePlayer
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.AttackedSubject as AttackedSubject
import qualified Parun.Types.PlayerRelation as PlayerRelation

codec :: Codec.Codec AttackedSubject.AttackedSubject
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Related" related (uncurry AttackedSubject.Related) (\x -> case x of AttackedSubject.Related relation kinds -> Just (relation, kinds); _ -> Nothing),
      Arm.payload "NamedBySource" SourcePlayer.codec AttackedSubject.NamedBySource (\x -> case x of AttackedSubject.NamedBySource y -> Just y; _ -> Nothing),
      Arm.nullary "Source" AttackedSubject.Source
    ]

tagOf :: AttackedSubject.AttackedSubject -> String
tagOf x = case x of
  AttackedSubject.Related {} -> "Related"
  AttackedSubject.NamedBySource {} -> "NamedBySource"
  AttackedSubject.Source -> "Source"

-- | Related's two keys, "kinds" defaulting to the player alone, as "attacks
-- you" and "attack a player" read.
--
-- Unprinted: a battle among the kinds, or none. Both are refused, and
-- Parun.Codec.AttackedSubjectSpec proves the refusals.
related :: Codec.Codec (PlayerRelation.PlayerRelation, Set AttackTargetKind.AttackTargetKind)
related = Fields.objectWith kindsArePrinted $ do
  relation <- Fields.required "relation" PlayerRelation.codec fst
  kinds <- Fields.defaulted "kinds" (Set.singleton AttackTargetKind.OfPlayer) (Common.set AttackTargetKind.codec) snd
  pure (relation, kinds)

kindsArePrinted :: (PlayerRelation.PlayerRelation, Set AttackTargetKind.AttackTargetKind) -> Either Text.Text (PlayerRelation.PlayerRelation, Set AttackTargetKind.AttackTargetKind)
kindsArePrinted x@(_, kinds)
  | Set.null kinds = Left (Text.pack "AttackedSubject: a Related subject admitting no kind is unprinted and not supported")
  | Set.member AttackTargetKind.OfBattle kinds = Left (Text.pack "AttackedSubject: a Related subject admitting a battle is unprinted and not supported")
  | otherwise = Right x
