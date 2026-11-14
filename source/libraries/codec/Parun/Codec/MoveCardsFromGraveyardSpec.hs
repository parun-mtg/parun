module Parun.Codec.MoveCardsFromGraveyardSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.MoveCardsFromGraveyard as MoveCardsFromGraveyard
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.MoveCardsFromGraveyard as MoveCardsFromGraveyard
import qualified Parun.Types.SlotName as SlotName

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (MoveCardsFromGraveyard.MoveCardsFromGraveyard Keyword.Keyword)
codec = MoveCardsFromGraveyard.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.MoveCardsFromGraveyard" $ do
  -- CR 406.2 as a cost: Headless Skaab exiles one creature card from your graveyard.
  Spec.it s "MkMoveCardsFromGraveyard" $
    Common.assertCodec
      s
      codec
      ( MoveCardsFromGraveyard.MkMoveCardsFromGraveyard
          { MoveCardsFromGraveyard.count = CostAmount.Fixed 1,
            MoveCardsFromGraveyard.whichCards = Filter.HasCardType CardType.Creature,
            MoveCardsFromGraveyard.destination = GraveyardCostDestination.Exile
          }
      )
      " {\"count\":{\"type\":\"Fixed\",\"value\":1},\"whichCards\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  -- Gorex, the Tombshell's "any number", announced at CR 601.2b.
  Spec.it s "Announced" $
    Common.assertCodec
      s
      codec
      ( MoveCardsFromGraveyard.MkMoveCardsFromGraveyard
          { MoveCardsFromGraveyard.count = CostAmount.Announced (SlotName.MkSlotName (Text.pack "X")),
            MoveCardsFromGraveyard.whichCards = Filter.HasCardType CardType.Creature,
            MoveCardsFromGraveyard.destination = GraveyardCostDestination.Exile
          }
      )
      " {\"count\":{\"type\":\"Announced\",\"value\":\"X\"},\"whichCards\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  -- Gurzigost's "put two cards from your graveyard on the bottom of your library".
  Spec.it s "LibraryBottom" $
    Common.assertCodec
      s
      codec
      ( MoveCardsFromGraveyard.MkMoveCardsFromGraveyard
          { MoveCardsFromGraveyard.count = CostAmount.Fixed 2,
            MoveCardsFromGraveyard.whichCards = Filter.And [],
            MoveCardsFromGraveyard.destination = GraveyardCostDestination.LibraryBottom
          }
      )
      " {\"count\":{\"type\":\"Fixed\",\"value\":2},\"destination\":\"LibraryBottom\",\"whichCards\":{\"type\":\"And\",\"value\":[]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
