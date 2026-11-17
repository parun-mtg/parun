module Parun.Codec.ExileCardsTotallingSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Parun.Codec.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Zone as Zone

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (ExileCardsTotalling.ExileCardsTotalling Keyword.Keyword)
codec = ExileCardsTotalling.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ExileCardsTotalling" $ do
  -- Baron Helmut Zemo's boast: black cards with fifteen or more black mana
  -- symbols, every defaulted field omitted.
  Spec.it s "MkExileCardsTotalling" $
    Common.assertCodec
      s
      codec
      ( ExileCardsTotalling.MkExileCardsTotalling
          { ExileCardsTotalling.threshold = CostAmount.Fixed 15,
            ExileCardsTotalling.comparison = Comparison.AtLeast,
            ExileCardsTotalling.fewest = 0,
            ExileCardsTotalling.tally = CardTally.ManaSymbolsOf Color.Black,
            ExileCardsTotalling.whichCards = Filter.HasColor Color.Black,
            ExileCardsTotalling.zone = Zone.Graveyard
          }
      )
      " {\"tally\":{\"type\":\"ManaSymbolsOf\",\"value\":\"Black\"},\"threshold\":{\"type\":\"Fixed\",\"value\":15},\"whichCards\":{\"type\":\"HasColor\",\"value\":\"Black\"}} "
  -- Fabrication Foundry's: one or more permanents totalling X exactly.
  Spec.it s "MkExileCardsTotalling with every field" $
    Common.assertCodec
      s
      codec
      ( ExileCardsTotalling.MkExileCardsTotalling
          { ExileCardsTotalling.threshold = CostAmount.Announced (SlotName.MkSlotName (Text.pack "X")),
            ExileCardsTotalling.comparison = Comparison.Exactly,
            ExileCardsTotalling.fewest = 1,
            ExileCardsTotalling.tally = CardTally.ManaValue,
            ExileCardsTotalling.whichCards = Filter.IsSource,
            ExileCardsTotalling.zone = Zone.Battlefield
          }
      )
      " {\"comparison\":\"Exactly\",\"fewest\":1,\"tally\":{\"type\":\"ManaValue\"},\"threshold\":{\"type\":\"Announced\",\"value\":\"X\"},\"whichCards\":{\"type\":\"IsSource\"},\"zone\":\"Battlefield\"} "
  -- Unprinted: a zone the engine reads no candidates from, so refused.
  Spec.it s "rejects a zone" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode codec =<< Common.parse (Text.pack "{\"tally\":{\"type\":\"ManaValue\"},\"threshold\":{\"type\":\"Fixed\",\"value\":1},\"whichCards\":{\"type\":\"IsSource\"},\"zone\":\"Hand\"}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
