module Parun.Codec.ExileCardsTotallingSpec where

import qualified Parun.Codec.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ExileCardsTotalling as ExileCardsTotalling
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | Instantiated at 'Keyword.Keyword', the only concrete instantiation anywhere
-- in the pool.
codec :: Codec.Codec (ExileCardsTotalling.ExileCardsTotalling Keyword.Keyword)
codec = ExileCardsTotalling.codec Keyword.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ExileCardsTotalling" $ do
  -- Baron Helmut Zemo's boast: black cards with fifteen or more black mana symbols.
  Spec.it s "MkExileCardsTotalling" $
    Common.assertCodec
      s
      codec
      ( ExileCardsTotalling.MkExileCardsTotalling
          { ExileCardsTotalling.threshold = 15,
            ExileCardsTotalling.tally = CardTally.ManaSymbolsOf Color.Black,
            ExileCardsTotalling.whichCards = Filter.HasColor Color.Black
          }
      )
      " {\"tally\":{\"type\":\"ManaSymbolsOf\",\"value\":\"Black\"},\"threshold\":15,\"whichCards\":{\"type\":\"HasColor\",\"value\":\"Black\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
