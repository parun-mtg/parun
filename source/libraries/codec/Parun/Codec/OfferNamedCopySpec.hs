module Parun.Codec.OfferNamedCopySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Parun.Codec.CastOffer as CastOffer
import qualified Parun.Codec.OfferNamedCopy as OfferNamedCopy
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.CopyChoice as CopyChoice
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.FromReference as FromReference
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.NamedCopySource as NamedCopySource
import qualified Parun.Types.OfferNamedCopy as OfferNamedCopy
import qualified Parun.Types.PayBranch as PayBranch
import qualified Parun.Types.PayGate as PayGate
import qualified Parun.Types.PayObligation as PayObligation
import qualified Parun.Types.PayerGrouping as PayerGrouping
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- Maelstrom Archangel Avatar's "You may pay {3}. If you do".
payThree :: PayGate.PayGate
payThree =
  PayGate.MkPayGate
    { PayGate.payer = PlayerRef.Relative PlayerRelation.You,
      PayGate.grouping = PayerGrouping.Severally,
      PayGate.cost = CostChoice.MkCostChoice (Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []} NonEmpty.:| []),
      PayGate.basis = Nothing,
      PayGate.branch = PayBranch.IfPaid,
      PayGate.obligation = PayObligation.Optional,
      PayGate.perEach = Nothing,
      PayGate.x = Nothing,
      PayGate.offeredAt = Nothing,
      PayGate.marks = Nothing
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OfferNamedCopy" $ do
  Spec.it s "elides the defaults" $
    Common.assertCodec
      s
      OfferNamedCopy.codec
      (OfferNamedCopy.MkOfferNamedCopy (NamedCopySource.Unchosen (pure (CardName.MkCardName (Text.pack "Terror")))) 1 CastOffer.defaultValue Nothing CopyChoice.CastOne)
      " {\"names\":{\"type\":\"Unchosen\",\"value\":[\"Terror\"]}} "
  Spec.it s "a count and an offer" $
    Common.assertCodec
      s
      OfferNamedCopy.codec
      (OfferNamedCopy.MkOfferNamedCopy (NamedCopySource.AtRandom (FromReference.MkFromReference (Filter.HasCardType CardType.Instant) Nothing)) 3 CastOffer.defaultValue {CastOffer.withoutPayingManaCost = True} Nothing CopyChoice.CastOne)
      " {\"names\":{\"type\":\"AtRandom\",\"value\":{\"filter\":{\"type\":\"HasCardType\",\"value\":\"Instant\"}}},\"count\":3,\"offer\":{\"withoutPayingManaCost\":true}} "
  Spec.it s "a cost and a choice" $
    Common.assertCodec
      s
      OfferNamedCopy.codec
      (OfferNamedCopy.MkOfferNamedCopy (NamedCopySource.Unchosen (pure (CardName.MkCardName (Text.pack "Terror")))) 1 CastOffer.defaultValue (Just payThree) CopyChoice.ChooseThenTokenOrCast)
      " {\"names\":{\"type\":\"Unchosen\",\"value\":[\"Terror\"]},\"payGate\":{\"payer\":{\"type\":\"Relative\",\"value\":\"You\"},\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"branch\":\"IfPaid\"},\"choice\":\"ChooseThenTokenOrCast\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s OfferNamedCopy.codec
