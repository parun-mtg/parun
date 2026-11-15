module Parun.Codec.TurnedFaceUpSpec where

import qualified Parun.Codec.TurnedFaceUp as TurnedFaceUp
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.TurnedFaceUp as TurnedFaceUp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TurnedFaceUp" $ do
  -- CR 107.3d: the X chosen for the cost, written out, beside the player who
  -- turned it.
  Spec.it s "MkTurnedFaceUp with a chosen X" $
    Common.assertCodec
      s
      TurnedFaceUp.codec
      (TurnedFaceUp.MkTurnedFaceUp {TurnedFaceUp.object = ObjectId.MkObjectId 5, TurnedFaceUp.player = PlayerId.MkPlayerId 1, TurnedFaceUp.announcedX = Just 2})
      "{\"object\":5,\"player\":1,\"announcedX\":2}"
  -- A zero X is a choice and survives the round trip, apart from the absent key.
  Spec.it s "MkTurnedFaceUp with X chosen as zero" $
    Common.assertCodec
      s
      TurnedFaceUp.codec
      (TurnedFaceUp.MkTurnedFaceUp {TurnedFaceUp.object = ObjectId.MkObjectId 5, TurnedFaceUp.player = PlayerId.MkPlayerId 1, TurnedFaceUp.announcedX = Just 0})
      "{\"object\":5,\"player\":1,\"announcedX\":0}"
  -- A road up with no X in its cost leaves the key out.
  Spec.it s "MkTurnedFaceUp with no X" $
    Common.assertCodec
      s
      TurnedFaceUp.codec
      (TurnedFaceUp.MkTurnedFaceUp {TurnedFaceUp.object = ObjectId.MkObjectId 5, TurnedFaceUp.player = PlayerId.MkPlayerId 1, TurnedFaceUp.announcedX = Nothing})
      "{\"object\":5,\"player\":1}"
  Spec.it s "has a schema" $ Common.assertHasSchema s TurnedFaceUp.codec
