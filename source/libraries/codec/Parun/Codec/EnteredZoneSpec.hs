module Parun.Codec.EnteredZoneSpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.EnteredZone as EnteredZone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EnteredZone as EnteredZone
import qualified Parun.Types.EntryWindow as EntryWindow
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.EnteredZone" $ do
  Spec.it s "MkEnteredZone, from anywhere" $
    Common.assertCodec
      s
      EnteredZone.codec
      (EnteredZone.MkEnteredZone {EnteredZone.to = Zone.Battlefield, EnteredZone.from = Set.empty, EnteredZone.window = EntryWindow.ThisTurn, EnteredZone.under = Nothing})
      " {\"to\":\"Battlefield\"} "
  -- Asymmetric on purpose, Parun.Codec.MovedBetweenSpec's reason: both keys
  -- carry a Zone.
  Spec.it s "MkEnteredZone, a named origin" $
    Common.assertCodec
      s
      EnteredZone.codec
      (EnteredZone.MkEnteredZone {EnteredZone.to = Zone.Graveyard, EnteredZone.from = Set.singleton Zone.Battlefield, EnteredZone.window = EntryWindow.ThisTurn, EnteredZone.under = Nothing})
      " {\"from\":[\"Battlefield\"],\"to\":\"Graveyard\"} "
  Spec.it s "MkEnteredZone, since your last turn ended" $
    Common.assertCodec
      s
      EnteredZone.codec
      (EnteredZone.MkEnteredZone {EnteredZone.to = Zone.Battlefield, EnteredZone.from = Set.empty, EnteredZone.window = EntryWindow.SinceOwnLastTurn, EnteredZone.under = Nothing})
      " {\"to\":\"Battlefield\",\"window\":\"SinceOwnLastTurn\"} "
  Spec.it s "MkEnteredZone, under your control" $
    Common.assertCodec
      s
      EnteredZone.codec
      (EnteredZone.MkEnteredZone {EnteredZone.to = Zone.Battlefield, EnteredZone.from = Set.empty, EnteredZone.window = EntryWindow.ThisTurn, EnteredZone.under = Just PlayerRelation.You})
      " {\"to\":\"Battlefield\",\"under\":\"You\"} "
  Spec.it s "rejects under beside another zone" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode EnteredZone.codec =<< Common.parse (Text.pack "{\"to\":\"Graveyard\",\"under\":\"You\"}")))
      "expected a decode failure"
  Spec.it s "rejects under beside another window" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode EnteredZone.codec =<< Common.parse (Text.pack "{\"to\":\"Battlefield\",\"under\":\"You\",\"window\":\"SinceOwnLastTurn\"}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s EnteredZone.codec
