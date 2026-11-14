module Parun.Codec.CardAbilitiesSpec where

import qualified Parun.Codec.CardAbilities as CardAbilities
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.CardAbilities as CardAbilities
import qualified Parun.Types.ExiledPile as ExiledPile
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.LentCards as LentCards

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CardAbilities" $ do
  Spec.it s "with restrictions" $
    Common.assertCodec
      s
      CardAbilities.codec
      CardAbilities.MkCardAbilities {CardAbilities.from = LentCards.InExile, CardAbilities.cards = Filter.IsExiledWithSource ExiledPile.CraftMaterials, CardAbilities.triggered = False, CardAbilities.only = Nothing, CardAbilities.restrictions = [ActivationRestriction.TimesEachTurn 1]}
      " {\"cards\":{\"type\":\"IsExiledWithSource\",\"value\":{\"type\":\"CraftMaterials\"}},\"restrictions\":[{\"type\":\"TimesEachTurn\",\"value\":1}]} "
  Spec.it s "restrictions default to none" $
    Common.assertCodec
      s
      CardAbilities.codec
      CardAbilities.MkCardAbilities {CardAbilities.from = LentCards.InExile, CardAbilities.cards = Filter.IsExiledWithSource ExiledPile.OwnAbilities, CardAbilities.triggered = False, CardAbilities.only = Nothing, CardAbilities.restrictions = []}
      " {\"cards\":{\"type\":\"IsExiledWithSource\",\"value\":{\"type\":\"OwnAbilities\"}}} "
  Spec.it s "walks the top of a library" $
    Common.assertCodec
      s
      CardAbilities.codec
      CardAbilities.MkCardAbilities {CardAbilities.from = LentCards.TopOfYourLibrary, CardAbilities.cards = Filter.And [], CardAbilities.triggered = False, CardAbilities.only = Nothing, CardAbilities.restrictions = []}
      " {\"from\":\"TopOfYourLibrary\",\"cards\":{\"type\":\"And\",\"value\":[]}} "
  Spec.it s "lends triggered abilities too" $
    Common.assertCodec
      s
      CardAbilities.codec
      CardAbilities.MkCardAbilities {CardAbilities.from = LentCards.InExile, CardAbilities.cards = Filter.IsExiledWithSource ExiledPile.LastChosen, CardAbilities.triggered = True, CardAbilities.only = Nothing, CardAbilities.restrictions = []}
      " {\"cards\":{\"type\":\"IsExiledWithSource\",\"value\":{\"type\":\"LastChosen\"}},\"triggered\":true} "
  Spec.it s "walks the battlefield and lends one kind" $
    Common.assertCodec
      s
      CardAbilities.codec
      CardAbilities.MkCardAbilities {CardAbilities.from = LentCards.OnTheBattlefield, CardAbilities.cards = Filter.And [], CardAbilities.triggered = False, CardAbilities.only = Just AbilityKind.NonManaAbility, CardAbilities.restrictions = []}
      " {\"from\":\"OnTheBattlefield\",\"cards\":{\"type\":\"And\",\"value\":[]},\"only\":\"NonManaAbility\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CardAbilities.codec
