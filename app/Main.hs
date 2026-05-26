{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Protolude (
  Generic,
  IO,
  Int,
  Maybe (..),
  Show,
  Text,
  die,
  drop,
  fmap,
  fromMaybe,
  pure,
  putText,
  show,
  toLower,
  ($),
  (&),
  (<>),
 )

import Data.Text qualified as T
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Generic (
  Modifiers (fieldNameModifier),
  ParseFields,
  ParseRecord (parseRecord),
  defaultModifiers,
  getRecord,
  parseRecordWithModifiers,
  type (<!>) (..),
  type (<?>) (..),
 )

import SQLiteDAV.Server (RowNameMode (..), parseRowNameMode, webDavServer)


modifiers :: Modifiers
modifiers =
  defaultModifiers{fieldNameModifier = fmap toLower}


data Options = Options
  { port :: Maybe Int -- "Port to listen on"
  , dbPath :: Text -- "Path to SQLite database file"
  , rowName :: Maybe Text
  -- ^ How to name row directories: @rowid@ (default), @pk@, or @combined@.
  }
  deriving (Show, Generic)


instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers


main :: IO ()
main = do
  (options :: Options) <- getRecord "SQLiteDAV server"
  let thePort = options.port & fromMaybe 1234
  rowMode <- case options.rowName of
    Nothing -> pure RowIdMode
    Just raw -> case parseRowNameMode raw of
      Just m -> pure m
      Nothing ->
        die $
          "Invalid --rowname value: "
            <> raw
            <> " (expected rowid, pk, or combined)"
  putText $ "Starting server on http://localhost:" <> show thePort
  run thePort $
    logStdoutDev $
      webDavServer rowMode (T.unpack options.dbPath)
