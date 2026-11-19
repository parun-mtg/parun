-- Reads scenarios from disk: data/scenarios, and any file a caller names.
module Parun.Scenario.Load where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Parun.Codec.Scenario as Scenario
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Trustworthy.Directory as Directory
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Paths_parun as Paths

-- | data/scenarios as cabal data-files, as Parun.Registry.defaultRoot finds
-- data/cards.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "scenarios"

-- | data/scenario-manifest.json, the scenarios worth looking at in the web
-- client's replay (Parun.Browser.Replay), by theme.
manifestPath :: IO FilePath
manifestPath = Paths.getDataFileName "scenario-manifest.json"

-- | Each theme's scenarios, a name beneath the root less its extension.
parseManifest :: ByteString.ByteString -> Either Text.Text (Map.Map Text.Text [Text.Text])
parseManifest bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Codec.decode (Common.textMap id Right (Common.list Common.text))

-- | Decoded as UTF-8 explicitly, for Parun.Registry.parseCard's reason.
parse :: ByteString.ByteString -> Either Text.Text Scenario.Type.Scenario
parse bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Codec.decode Scenario.codec

-- | Every .json file beneath a root, ascending by path, read no further: a
-- caller decodes each with 'loadFile'.
listRoot :: FilePath -> IO [FilePath]
listRoot = fmap List.sort . jsonFiles

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles dir = do
  entries <- Directory.listDirectory dir
  fmap concat . Monad.forM entries $ \entry -> do
    let path = dir <> "/" <> entry
    nested <- Directory.doesDirectoryExist path
    if nested
      then jsonFiles path
      else pure [path | List.isSuffixOf ".json" entry]

loadFile :: FilePath -> IO (FilePath, Either Text.Text Scenario.Type.Scenario)
loadFile path = fmap (\bytes -> (path, parse bytes)) (ByteString.readFile path)
