module Parun.Codec.BeholdSpec where

import qualified Parun.Codec.Behold as Behold
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Behold as Behold
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.SubtypeFamily as SubtypeFamily
import qualified Parun.Types.Zone as Zone

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (Behold.Behold Keyword.Keyword)
codec = Behold.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Behold" $ do
  -- Kindle the Inner Flame's three Elementals.
  Spec.it s "MkBehold" $
    Common.assertCodec
      s
      codec
      (Behold.MkBehold {Behold.count = 3, Behold.whichObjects = Filter.HasSubtype Subtype.Elemental, Behold.chosenType = Nothing, Behold.cardZone = Zone.Hand})
      " {\"count\":3,\"whichObjects\":{\"type\":\"HasSubtype\",\"value\":\"Elemental\"}} "
  -- Celestial Reunion's two creatures of a type the payer chooses.
  Spec.it s "MkBehold choosing a type" $
    Common.assertCodec
      s
      codec
      (Behold.MkBehold {Behold.count = 2, Behold.whichObjects = Filter.HasChosenSubtype, Behold.chosenType = Just SubtypeFamily.CreatureType, Behold.cardZone = Zone.Hand})
      " {\"chosenType\":\"CreatureType\",\"count\":2,\"whichObjects\":{\"type\":\"HasChosenSubtype\"}} "
  -- Close Encounter's creature you control or warped creature card in exile.
  Spec.it s "MkBehold from exile" $
    Common.assertCodec
      s
      codec
      (Behold.MkBehold {Behold.count = 1, Behold.whichObjects = Filter.IsWarped, Behold.chosenType = Nothing, Behold.cardZone = Zone.Exile})
      " {\"cardZone\":\"Exile\",\"count\":1,\"whichObjects\":{\"type\":\"IsWarped\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
