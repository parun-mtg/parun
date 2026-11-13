module Parun.Codec.ExiledPile where

import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.CardName as CardName
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ExiledPile as ExiledPile

codec :: Codec.Codec ExiledPile.ExiledPile
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "OwnAbilities" ExiledPile.OwnAbilities,
      Arm.payload "OfAbility" AbilityName.codec ExiledPile.OfAbility (\x -> case x of ExiledPile.OfAbility y -> Just y; _ -> Nothing),
      Arm.nullary "AsItEntered" ExiledPile.AsItEntered,
      Arm.nullary "CraftMaterials" ExiledPile.CraftMaterials,
      Arm.payload "WithCardsNamed" CardName.codec ExiledPile.WithCardsNamed (\x -> case x of ExiledPile.WithCardsNamed y -> Just y; _ -> Nothing)
    ]

tagOf :: ExiledPile.ExiledPile -> String
tagOf x = case x of
  ExiledPile.OwnAbilities -> "OwnAbilities"
  ExiledPile.OfAbility {} -> "OfAbility"
  ExiledPile.AsItEntered -> "AsItEntered"
  ExiledPile.CraftMaterials -> "CraftMaterials"
  ExiledPile.WithCardsNamed {} -> "WithCardsNamed"
