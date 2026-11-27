module Parun.Codec.AnchorSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Anchor as Anchor
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.AnchorPool as AnchorPool
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.Keyword as Keyword.Type
import qualified Parun.Types.SelfInclusion as SelfInclusion
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Anchor" $ do
  Spec.it s "Bound" $
    Common.assertCodec
      s
      codec
      (Anchor.Bound Nothing (SlotName.MkSlotName (Text.pack "thatDepartedPermanent")))
      " {\"type\":\"Bound\",\"value\":{\"slot\":\"thatDepartedPermanent\"}} "
  Spec.it s "Bound narrowed to the candidate's controller" $
    Common.assertCodec
      s
      codec
      (Anchor.Bound (Just Holder.Controller) (SlotName.MkSlotName (Text.pack "creature")))
      " {\"type\":\"Bound\",\"value\":{\"heldAlike\":\"Controller\",\"slot\":\"creature\"}} "
  Spec.it s "Evaluated" $
    Common.assertCodec
      s
      codec
      Anchor.Evaluated
      " {\"type\":\"Evaluated\"} "
  Spec.it s "Source" $
    Common.assertCodec
      s
      codec
      Anchor.Source
      " {\"type\":\"Source\"} "
  Spec.it s "AnyPermanent" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.Permanents SelfInclusion.IncludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyPermanent\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "AnyOtherPermanent" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.Permanents SelfInclusion.ExcludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyOtherPermanent\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "AnyExiledCard" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.ExiledCards SelfInclusion.IncludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyExiledCard\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "AnyOtherExiledCard" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.ExiledCards SelfInclusion.ExcludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyOtherExiledCard\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "AnyGraveyardCard" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.GraveyardCards SelfInclusion.IncludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyGraveyardCard\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "AnyOtherGraveyardCard" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.GraveyardCards SelfInclusion.ExcludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyOtherGraveyardCard\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "AnyLibraryTop" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.LibraryTops SelfInclusion.IncludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyLibraryTop\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "AnyOtherLibraryTop" $
    Common.assertCodec
      s
      codec
      (Anchor.AnyObject AnchorPool.LibraryTops SelfInclusion.ExcludesCandidate (Filter.Type.HasCardType CardType.Creature))
      " {\"type\":\"AnyOtherLibraryTop\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "YourCommander" $
    Common.assertCodec
      s
      codec
      Anchor.YourCommander
      " {\"type\":\"YourCommander\"} "
  Spec.it s "Host" $
    Common.assertCodec
      s
      codec
      Anchor.Host
      " {\"type\":\"Host\"} "

codec :: Codec.Codec (Anchor.Anchor (Filter.Type.Filter Keyword.Type.Keyword))
codec = Anchor.codec (Filter.codec Keyword.codec)
