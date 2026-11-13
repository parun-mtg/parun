module Parun.Codec.PregameExileSpec where

import qualified Parun.Codec.PregameExile as PregameExile
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PregameExile as PregameExile

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PregameExile" $ do
  Spec.it s "one card, the default" $
    Common.assertCodec s PregameExile.codec (PregameExile.MkPregameExile (Filter.HasCardType CardType.Creature) 1 (Just 1)) " {\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "three or more" $
    Common.assertCodec s PregameExile.codec (PregameExile.MkPregameExile (Filter.HasCardType CardType.Creature) 3 Nothing) " {\"filter\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"atLeast\":3,\"atMost\":null} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PregameExile.codec
