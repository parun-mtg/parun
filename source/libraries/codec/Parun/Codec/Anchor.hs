module Parun.Codec.Anchor where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Holder as Holder
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.Holder as Holder.Type
import qualified Parun.Types.SlotName as SlotName.Type

-- | Parametric in the filter's codec, which Parun.Codec.Filter supplies.
codec :: (Typeable.Typeable nested, Eq nested) => Codec.Codec nested -> Codec.Codec (Anchor.Anchor nested)
codec filterCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "Bound" bound (uncurry Anchor.Bound) (\x -> case x of Anchor.Bound h y -> Just (h, y); _ -> Nothing),
      Arm.nullary "Evaluated" Anchor.Evaluated,
      Arm.nullary "Source" Anchor.Source,
      Arm.payload "AnyPermanent" filterCodec Anchor.AnyPermanent (\x -> case x of Anchor.AnyPermanent y -> Just y; _ -> Nothing),
      Arm.nullary "YourCommander" Anchor.YourCommander,
      Arm.nullary "Host" Anchor.Host
    ]

-- | The slot, and the holder its members are narrowed by, absent for the whole group.
bound :: Codec.Codec (Maybe Holder.Type.Holder, SlotName.Type.SlotName)
bound =
  Fields.object $
    (,)
      <$> Fields.defaulted "heldAlike" Nothing (Common.maybe Holder.codec) fst
      <*> Fields.required "slot" SlotName.codec snd

tagOf :: Anchor.Anchor nested -> String
tagOf x = case x of
  Anchor.Bound {} -> "Bound"
  Anchor.Evaluated {} -> "Evaluated"
  Anchor.Source {} -> "Source"
  Anchor.AnyPermanent {} -> "AnyPermanent"
  Anchor.YourCommander {} -> "YourCommander"
  Anchor.Host {} -> "Host"
