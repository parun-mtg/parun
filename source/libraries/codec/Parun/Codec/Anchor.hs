module Parun.Codec.Anchor where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Holder as Holder
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Anchor as Anchor
import qualified Parun.Types.AnchorPool as AnchorPool
import qualified Parun.Types.Holder as Holder.Type
import qualified Parun.Types.SelfInclusion as SelfInclusion
import qualified Parun.Types.SlotName as SlotName.Type

-- | Parametric in the filter's codec, which Parun.Codec.Filter supplies.
codec :: (Typeable.Typeable nested, Eq nested) => Codec.Codec nested -> Codec.Codec (Anchor.Anchor nested)
codec filterCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "Bound" bound (uncurry Anchor.Bound) (\x -> case x of Anchor.Bound h y -> Just (h, y); _ -> Nothing),
      Arm.nullary "Evaluated" Anchor.Evaluated,
      Arm.nullary "Source" Anchor.Source,
      anyObject "AnyPermanent" AnchorPool.Permanents SelfInclusion.IncludesCandidate,
      anyObject "AnyOtherPermanent" AnchorPool.Permanents SelfInclusion.ExcludesCandidate,
      anyObject "AnyExiledCard" AnchorPool.ExiledCards SelfInclusion.IncludesCandidate,
      anyObject "AnyOtherExiledCard" AnchorPool.ExiledCards SelfInclusion.ExcludesCandidate,
      anyObject "AnyGraveyardCard" AnchorPool.GraveyardCards SelfInclusion.IncludesCandidate,
      anyObject "AnyOtherGraveyardCard" AnchorPool.GraveyardCards SelfInclusion.ExcludesCandidate,
      anyObject "AnyLibraryTop" AnchorPool.LibraryTops SelfInclusion.IncludesCandidate,
      anyObject "AnyOtherLibraryTop" AnchorPool.LibraryTops SelfInclusion.ExcludesCandidate,
      Arm.nullary "YourCommander" Anchor.YourCommander,
      Arm.nullary "Host" Anchor.Host
    ]
  where
    anyObject tag pool inclusion = Arm.payload tag filterCodec (Anchor.AnyObject pool inclusion) (\x -> case x of Anchor.AnyObject p i y | p == pool, i == inclusion -> Just y; _ -> Nothing)

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
  Anchor.AnyObject AnchorPool.Permanents SelfInclusion.IncludesCandidate _ -> "AnyPermanent"
  Anchor.AnyObject AnchorPool.Permanents SelfInclusion.ExcludesCandidate _ -> "AnyOtherPermanent"
  Anchor.AnyObject AnchorPool.ExiledCards SelfInclusion.IncludesCandidate _ -> "AnyExiledCard"
  Anchor.AnyObject AnchorPool.ExiledCards SelfInclusion.ExcludesCandidate _ -> "AnyOtherExiledCard"
  Anchor.AnyObject AnchorPool.GraveyardCards SelfInclusion.IncludesCandidate _ -> "AnyGraveyardCard"
  Anchor.AnyObject AnchorPool.GraveyardCards SelfInclusion.ExcludesCandidate _ -> "AnyOtherGraveyardCard"
  Anchor.AnyObject AnchorPool.LibraryTops SelfInclusion.IncludesCandidate _ -> "AnyLibraryTop"
  Anchor.AnyObject AnchorPool.LibraryTops SelfInclusion.ExcludesCandidate _ -> "AnyOtherLibraryTop"
  Anchor.YourCommander {} -> "YourCommander"
  Anchor.Host {} -> "Host"
