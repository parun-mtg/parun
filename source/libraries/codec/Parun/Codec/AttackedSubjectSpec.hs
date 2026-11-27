module Parun.Codec.AttackedSubjectSpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.AttackedSubject as AttackedSubject
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackTargetKind as AttackTargetKind
import qualified Parun.Types.AttackedSubject as AttackedSubject
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SourcePlayer as SourcePlayer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AttackedSubject" $ do
  Spec.it s "Related" $
    Common.assertCodec s AttackedSubject.codec (AttackedSubject.Related PlayerRelation.You (Set.singleton AttackTargetKind.OfPlayer)) " {\"type\":\"Related\",\"value\":{\"relation\":\"You\"}} "
  Spec.it s "Related with a planeswalker" $
    Common.assertCodec s AttackedSubject.codec (AttackedSubject.Related PlayerRelation.AnyPlayer (Set.fromList [AttackTargetKind.OfPlayer, AttackTargetKind.OfPlaneswalker])) " {\"type\":\"Related\",\"value\":{\"kinds\":[\"OfPlayer\",\"OfPlaneswalker\"],\"relation\":\"AnyPlayer\"}} "
  Spec.it s "rejects a battle" $ rejects "{\"type\":\"Related\",\"value\":{\"kinds\":[\"OfPlayer\",\"OfBattle\"],\"relation\":\"AnyPlayer\"}}"
  Spec.it s "rejects no kind" $ rejects "{\"type\":\"Related\",\"value\":{\"kinds\":[],\"relation\":\"AnyPlayer\"}}"
  Spec.it s "NamedBySource Enchanted" $
    Common.assertCodec s AttackedSubject.codec (AttackedSubject.NamedBySource SourcePlayer.Enchanted) " {\"type\":\"NamedBySource\",\"value\":{\"type\":\"Enchanted\"}} "
  Spec.it s "NamedBySource Chosen" $
    Common.assertCodec s AttackedSubject.codec (AttackedSubject.NamedBySource SourcePlayer.Chosen) " {\"type\":\"NamedBySource\",\"value\":{\"type\":\"Chosen\"}} "
  Spec.it s "Source" $
    Common.assertCodec s AttackedSubject.codec AttackedSubject.Source " {\"type\":\"Source\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackedSubject.codec
  where
    rejects json =
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode AttackedSubject.codec =<< Common.parse (Text.pack json)))
        "expected a decode failure"
