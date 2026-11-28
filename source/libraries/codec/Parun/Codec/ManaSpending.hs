module Parun.Codec.ManaSpending where

import qualified Data.Maybe as Maybe
import qualified Parun.Codec.ManaFilter as ManaFilter
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ManaFilter as ManaFilter.Type
import qualified Parun.Types.ManaSpending as ManaSpending

-- | Keyed, so a nullary arm stays the bare string 'Arm.enum' wrote before
-- AnyColor grew a filter, and AnyColor over every mana stays bare "AnyColor".
codec :: Codec.Codec ManaSpending.ManaSpending
codec =
  Arm.keyed
    tagOf
    [ Arm.nullary "AsProduced" ManaSpending.AsProduced,
      Arm.nullary "AnyType" ManaSpending.AnyType,
      Arm.optionalPayload "AnyColor" ManaFilter.codec (ManaSpending.AnyColor . Maybe.fromMaybe ManaFilter.Type.Any) anyColor
    ]

anyColor :: ManaSpending.ManaSpending -> Maybe (Maybe ManaFilter.Type.ManaFilter)
anyColor x = case x of
  ManaSpending.AnyColor ManaFilter.Type.Any -> Just Nothing
  ManaSpending.AnyColor which -> Just (Just which)
  _ -> Nothing

tagOf :: ManaSpending.ManaSpending -> String
tagOf x = case x of
  ManaSpending.AsProduced -> "AsProduced"
  ManaSpending.AnyType -> "AnyType"
  ManaSpending.AnyColor {} -> "AnyColor"
