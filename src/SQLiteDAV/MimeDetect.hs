module SQLiteDAV.MimeDetect (detectMimeType, extensionForMime) where

import Protolude (
  IO,
  Maybe (..),
  otherwise,
  pure,
  ($),
  (.),
  (/=),
 )

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Unsafe qualified as BSU
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Magic (Magic)
import Magic.Data (MagicFlag (MagicMimeType))
import Magic.Init (magicLoadDefault, magicOpen)
import Magic.Operations (magicCString)
import Network.Mime (defaultExtensionMap)
import System.IO.Unsafe (unsafePerformIO)


-- libmagic handles are not thread-safe, so a single shared handle is
-- serialized through an MVar. Loading the magic database is expensive,
-- so we cache the handle for the lifetime of the process.
{-# NOINLINE globalMagic #-}
globalMagic :: MVar Magic
globalMagic = unsafePerformIO $ do
  magic <- magicOpen [MagicMimeType]
  magicLoadDefault magic
  newMVar magic


{-| Identify the MIME type of a byte buffer using libmagic.
Returns "application/octet-stream" for empty input.
-}
detectMimeType :: ByteString -> IO Text
detectMimeType bs
  | BS.null bs = pure "application/octet-stream"
  | otherwise =
      withMVar globalMagic $ \magic -> do
        result <- BSU.unsafeUseAsCStringLen bs (magicCString magic)
        pure $ T.pack result


{-| Canonical file extension for a MIME type, e.g.
"image/png" -> Just "png". Returns Nothing if unknown.
-}
extensionForMime :: Text -> Maybe Text
extensionForMime mime =
  let
    bare = BSC.takeWhile (/= ';') (BSC.pack $ T.unpack mime)
  in
    case Map.lookup bare defaultExtensionMap of
      Just (ext : _) -> Just ext
      _ -> Nothing
