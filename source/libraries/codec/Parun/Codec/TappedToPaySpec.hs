module Parun.Codec.TappedToPaySpec where

import qualified Parun.Codec.TappedToPay as TappedToPay
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.TappedToPay as TappedToPay

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TappedToPay" $ do
  Spec.it s "MkTappedToPay, both keys" $
    Common.assertCodec
      s
      TappedToPay.codec
      ( TappedToPay.MkTappedToPay
          { TappedToPay.permanent = ObjectId.MkObjectId 7,
            TappedToPay.keyword = KeywordFamily.Teamwork
          }
      )
      " {\"keyword\":\"Teamwork\",\"permanent\":7} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TappedToPay.codec
