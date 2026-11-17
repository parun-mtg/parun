module Parun.Codec.ExileMaterialsSpec where

import qualified Parun.Codec.ExileMaterials as ExileMaterials
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ExileMaterials as ExileMaterials
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Subtype as Subtype

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (ExileMaterials.ExileMaterials Keyword.Keyword)
codec = ExileMaterials.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ExileMaterials" $ do
  -- CR 702.167a: Tithing Blade's craft exiles one creature, which CR 702.167b
  -- reads across the battlefield and the graveyard at once.
  Spec.it s "MkExileMaterials" $
    Common.assertCodec
      s
      codec
      ( ExileMaterials.MkExileMaterials
          { ExileMaterials.count = 1,
            ExileMaterials.orMore = False,
            ExileMaterials.whichObjects = Filter.HasCardType CardType.Creature,
            ExileMaterials.oneEach = []
          }
      )
      " {\"count\":1,\"whichObjects\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  -- CR 702.167a: Paleontologist's Pick-Axe's craft exiles one or more creatures,
  -- so the count is a minimum and the key is written.
  Spec.it s "MkExileMaterials with orMore" $
    Common.assertCodec
      s
      codec
      ( ExileMaterials.MkExileMaterials
          { ExileMaterials.count = 1,
            ExileMaterials.orMore = True,
            ExileMaterials.whichObjects = Filter.HasCardType CardType.Creature,
            ExileMaterials.oneEach = []
          }
      )
      " {\"count\":1,\"orMore\":true,\"whichObjects\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  -- CR 702.167a: Throne of the Grim Captain's materials are a list, each met by
  -- a different object, so the key is written.
  Spec.it s "MkExileMaterials with oneEach" $
    Common.assertCodec
      s
      codec
      ( ExileMaterials.MkExileMaterials
          { ExileMaterials.count = 2,
            ExileMaterials.orMore = False,
            ExileMaterials.whichObjects = Filter.Or [Filter.HasSubtype Subtype.Dinosaur, Filter.HasSubtype Subtype.Pirate],
            ExileMaterials.oneEach = [Filter.HasSubtype Subtype.Dinosaur, Filter.HasSubtype Subtype.Pirate]
          }
      )
      " {\"count\":2,\"oneEach\":[{\"type\":\"HasSubtype\",\"value\":\"Dinosaur\"},{\"type\":\"HasSubtype\",\"value\":\"Pirate\"}],\"whichObjects\":{\"type\":\"Or\",\"value\":[{\"type\":\"HasSubtype\",\"value\":\"Dinosaur\"},{\"type\":\"HasSubtype\",\"value\":\"Pirate\"}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
