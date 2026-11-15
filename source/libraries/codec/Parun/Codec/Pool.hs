module Parun.Codec.Pool where

import qualified Parun.Codec.ZoneScope as ZoneScope
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.GraveyardPartner as GraveyardPartner
import qualified Parun.Types.Pool as Pool

-- | Tagged rather than nullary-only, because CR 400.1's per-player graveyard
-- makes one arm carry a payload: the ZoneScope saying whose. The nullary arms
-- are unaffected, since Common.nullary IS Common.tagged with no value.
codec :: Codec.Codec Pool.Pool
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Creatures" Pool.Creatures,
      Arm.nullary "Players" Pool.Players,
      Arm.nullary "AnyTarget" Pool.AnyTarget,
      Arm.nullary "Permanents" Pool.Permanents,
      Arm.nullary "Spells" Pool.Spells,
      Arm.nullary "Abilities" Pool.Abilities,
      Arm.nullary "SpellsAndPermanents" Pool.SpellsAndPermanents,
      Arm.nullary "SpellsAndAbilities" Pool.SpellsAndAbilities,
      Arm.nullary "PlayersAndPlaneswalkers" Pool.PlayersAndPlaneswalkers,
      Arm.nullary "PermanentsAndPlayers" Pool.PermanentsAndPlayers,
      Arm.payload "CardsInGraveyard" ZoneScope.codec Pool.CardsInGraveyard (\x -> case x of Pool.CardsInGraveyard y -> Just y; _ -> Nothing),
      -- Nullary: CR 400.1's shared zones have no per-player copy for a payload
      -- to select among.
      Arm.nullary "CardsInExile" Pool.CardsInExile,
      Arm.nullary "CardsInAnte" Pool.CardsInAnte,
      -- One arm per GraveyardPartner, told apart by the tag alone as
      -- Parun.Codec.ZoneScope's slot-named arms are: each carries only the
      -- graveyard half's ZoneScope.
      partnerArm "CreaturesAndCardsInGraveyard" GraveyardPartner.BattlefieldCreatures,
      partnerArm "CardsInGraveyardAndExile" GraveyardPartner.ExiledCards
    ]
  where
    partnerArm tag partner =
      Arm.payload tag ZoneScope.codec (Pool.CardsInGraveyardAnd partner) (\x -> case x of Pool.CardsInGraveyardAnd p y | p == partner -> Just y; _ -> Nothing)

tagOf :: Pool.Pool -> String
tagOf x = case x of
  Pool.Creatures {} -> "Creatures"
  Pool.Players {} -> "Players"
  Pool.AnyTarget {} -> "AnyTarget"
  Pool.Permanents {} -> "Permanents"
  Pool.Spells {} -> "Spells"
  Pool.Abilities {} -> "Abilities"
  Pool.SpellsAndPermanents {} -> "SpellsAndPermanents"
  Pool.SpellsAndAbilities {} -> "SpellsAndAbilities"
  Pool.PlayersAndPlaneswalkers {} -> "PlayersAndPlaneswalkers"
  Pool.PermanentsAndPlayers {} -> "PermanentsAndPlayers"
  Pool.CardsInGraveyard {} -> "CardsInGraveyard"
  Pool.CardsInExile {} -> "CardsInExile"
  Pool.CardsInAnte {} -> "CardsInAnte"
  Pool.CardsInGraveyardAnd partner _ -> case partner of
    GraveyardPartner.BattlefieldCreatures -> "CreaturesAndCardsInGraveyard"
    GraveyardPartner.ExiledCards -> "CardsInGraveyardAndExile"
