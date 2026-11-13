module Parun.Codec.EntryWindowSpec where

import qualified Parun.Codec.EntryWindow as EntryWindow
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EntryWindow as EntryWindow

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.EntryWindow" $ do
  Spec.it s "ThisTurn" $
    Common.assertCodec
      s
      EntryWindow.codec
      EntryWindow.ThisTurn
      " \"ThisTurn\" "
  Spec.it s "SinceOwnLastTurn" $
    Common.assertCodec
      s
      EntryWindow.codec
      EntryWindow.SinceOwnLastTurn
      " \"SinceOwnLastTurn\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s EntryWindow.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s EntryWindow.codec
