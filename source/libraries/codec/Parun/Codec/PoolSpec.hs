module Parun.Codec.PoolSpec where

import qualified Parun.Codec.Pool as Pool
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.GraveyardPartner as GraveyardPartner
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerScope as PlayerScope
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.ZoneScope as ZoneScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Pool" $ do
  Spec.it s "Creatures" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Creatures
      " {\"type\":\"Creatures\"} "
  Spec.it s "Players" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Players
      " {\"type\":\"Players\"} "
  Spec.it s "AnyTarget" $
    Common.assertCodec
      s
      Pool.codec
      Pool.AnyTarget
      " {\"type\":\"AnyTarget\"} "
  Spec.it s "Permanents" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Permanents
      " {\"type\":\"Permanents\"} "
  Spec.it s "Spells" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Spells
      " {\"type\":\"Spells\"} "
  -- CR 113.9: activated and triggered abilities on the stack, disjoint from
  -- Spells.
  Spec.it s "Abilities" $
    Common.assertCodec
      s
      Pool.codec
      Pool.Abilities
      " {\"type\":\"Abilities\"} "
  Spec.it s "SpellsAndPermanents" $
    Common.assertCodec
      s
      Pool.codec
      Pool.SpellsAndPermanents
      " {\"type\":\"SpellsAndPermanents\"} "
  Spec.it s "SpellsAndAbilities" $
    Common.assertCodec
      s
      Pool.codec
      Pool.SpellsAndAbilities
      " {\"type\":\"SpellsAndAbilities\"} "
  -- CR 115.2: battlefield planeswalkers plus clause (a)'s "or a player".
  Spec.it s "PlayersAndPlaneswalkers" $
    Common.assertCodec
      s
      Pool.codec
      Pool.PlayersAndPlaneswalkers
      " {\"type\":\"PlayersAndPlaneswalkers\"} "
  -- CR 115.2: battlefield permanents plus clause (a)'s "or a player".
  Spec.it s "PermanentsAndPlayers" $
    Common.assertCodec
      s
      Pool.codec
      Pool.PermanentsAndPlayers
      " {\"type\":\"PermanentsAndPlayers\"} "
  -- CR 404.1: the cards in a graveyard, tagged with WHOSE.
  Spec.it s "CardsInGraveyard" $
    Common.assertCodec
      s
      Pool.codec
      (Pool.CardsInGraveyard (ZoneScope.Scoped (PlayerScope.Related PlayerRelation.You)))
      " {\"type\":\"CardsInGraveyard\",\"value\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}}} "
  -- CR 406.1: the cards in the exile zone. Nullary, since exile has no
  -- per-player copy.
  Spec.it s "CardsInExile" $
    Common.assertCodec
      s
      Pool.codec
      Pool.CardsInExile
      " {\"type\":\"CardsInExile\"} "
  -- CR 407: the cards in the ante, nullary for CardsInExile's reason.
  Spec.it s "CardsInAnte" $
    Common.assertCodec
      s
      Pool.codec
      Pool.CardsInAnte
      " {\"type\":\"CardsInAnte\"} "
  -- The battlefield and CR 404.1's graveyard in one slot, so the graveyard half's
  -- scope is what the payload carries.
  Spec.it s "CreaturesAndCardsInGraveyard" $
    Common.assertCodec
      s
      Pool.codec
      (Pool.CardsInGraveyardAnd GraveyardPartner.BattlefieldCreatures (ZoneScope.Scoped (PlayerScope.Related PlayerRelation.AnyPlayer)))
      " {\"type\":\"CreaturesAndCardsInGraveyard\",\"value\":{\"type\":\"Scoped\",\"value\":{\"type\":\"AnyPlayer\"}}} "
  -- CR 404.1's graveyard and CR 406.1's exile in one slot, told apart from the
  -- arm above by the tag alone.
  Spec.it s "CardsInGraveyardAndExile" $
    Common.assertCodec
      s
      Pool.codec
      (Pool.CardsInGraveyardAnd GraveyardPartner.ExiledCards (ZoneScope.Scoped (PlayerScope.Related PlayerRelation.You)))
      " {\"type\":\"CardsInGraveyardAndExile\",\"value\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}}} "
  -- CR 112.1's stack and CR 110.1's battlefield beside a graveyard, the tag alone
  -- again.
  Spec.it s "SpellsPermanentsAndCardsInGraveyard" $
    Common.assertCodec
      s
      Pool.codec
      (Pool.CardsInGraveyardAnd GraveyardPartner.SpellsAndPermanents (ZoneScope.Scoped (PlayerScope.Related PlayerRelation.AnyPlayer)))
      " {\"type\":\"SpellsPermanentsAndCardsInGraveyard\",\"value\":{\"type\":\"Scoped\",\"value\":{\"type\":\"AnyPlayer\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Pool.codec
