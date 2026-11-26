module Parun.Codec.NamedCopySourceSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Parun.Codec.NamedCopySource as NamedCopySource
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FromReference as FromReference
import qualified Parun.Types.NamedCopySource as NamedCopySource

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.NamedCopySource" $ do
  Spec.it s "Unchosen" $
    Common.assertCodec
      s
      NamedCopySource.codec
      (NamedCopySource.Unchosen (CardName.MkCardName (Text.pack "Disenchant") NonEmpty.:| [CardName.MkCardName (Text.pack "Terror")]))
      " {\"type\":\"Unchosen\",\"value\":[\"Disenchant\",\"Terror\"]} "
  Spec.it s "AtRandom" $
    Common.assertCodec
      s
      NamedCopySource.codec
      (NamedCopySource.AtRandom (FromReference.MkFromReference (Filter.HasCardType CardType.Instant) Nothing))
      " {\"type\":\"AtRandom\",\"value\":{\"filter\":{\"type\":\"HasCardType\",\"value\":\"Instant\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s NamedCopySource.codec
