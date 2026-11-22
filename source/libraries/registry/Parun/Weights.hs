-- | The search bot's evaluation weights parun ships, each a file of
-- Parun.Codec.Weights in data/weights.
module Parun.Weights where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Parun.Codec.Weights as Weights.Codec
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.Weights as Weights
import qualified Paths_parun as Paths

-- | The weights the search bot plays with unless told otherwise.
defaultPath :: IO FilePath
defaultPath = Paths.getDataFileName "weights/default.json"

-- | The weights a file holds, or why it holds none.
load :: FilePath -> IO (Either Text.Text Weights.Weights)
load path = do
  bytes <- ByteString.readFile path
  pure $ do
    contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
    Common.parse contents >>= Codec.decode Weights.Codec.codec
