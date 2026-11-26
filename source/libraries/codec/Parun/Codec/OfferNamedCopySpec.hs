module Parun.Codec.OfferNamedCopySpec where

import qualified Data.Text as Text
import qualified Parun.Codec.CastOffer as CastOffer
import qualified Parun.Codec.OfferNamedCopy as OfferNamedCopy
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FromReference as FromReference
import qualified Parun.Types.NamedCopySource as NamedCopySource
import qualified Parun.Types.OfferNamedCopy as OfferNamedCopy

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OfferNamedCopy" $ do
  Spec.it s "elides the defaults" $
    Common.assertCodec
      s
      OfferNamedCopy.codec
      (OfferNamedCopy.MkOfferNamedCopy (NamedCopySource.Unchosen (pure (CardName.MkCardName (Text.pack "Terror")))) 1 CastOffer.defaultValue)
      " {\"names\":{\"type\":\"Unchosen\",\"value\":[\"Terror\"]}} "
  Spec.it s "a count and an offer" $
    Common.assertCodec
      s
      OfferNamedCopy.codec
      (OfferNamedCopy.MkOfferNamedCopy (NamedCopySource.AtRandom (FromReference.MkFromReference (Filter.HasCardType CardType.Instant) Nothing)) 3 CastOffer.defaultValue {CastOffer.withoutPayingManaCost = True})
      " {\"names\":{\"type\":\"AtRandom\",\"value\":{\"filter\":{\"type\":\"HasCardType\",\"value\":\"Instant\"}}},\"count\":3,\"offer\":{\"withoutPayingManaCost\":true}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s OfferNamedCopy.codec
