module Parun.Codec.DraftNoteTallySpec where

import qualified Data.Text as Text
import qualified Parun.Codec.DraftNoteTally as DraftNoteTally
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.DraftNoteTally as DraftNoteTally
import qualified Parun.Types.NoteFold as NoteFold
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DraftNoteTally" $ do
  Spec.it s "MkDraftNoteTally" $
    Common.assertCodec
      s
      DraftNoteTally.codec
      ( DraftNoteTally.MkDraftNoteTally
          { DraftNoteTally.player = PlayerRef.Relative PlayerRelation.You,
            DraftNoteTally.card = CardName.MkCardName (Text.pack "Lurking Automaton"),
            DraftNoteTally.fold = NoteFold.Highest
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"},\"card\":\"Lurking Automaton\",\"fold\":\"Highest\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DraftNoteTally.codec
