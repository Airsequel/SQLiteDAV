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
  drop,
  fmap,
  fromMaybe,
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

import SQLiteDAV.Server (webDavServer)


modifiers :: Modifiers
modifiers =
  defaultModifiers{fieldNameModifier = fmap toLower}


data Options = Options
  { port :: Maybe Int -- "Port to listen on"
  , dbPath :: Text -- "Path to SQLite database file"
  }
  deriving (Show, Generic)


instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers modifiers


main :: IO ()
main = do
  (options :: Options) <- getRecord "SQLiteDAV server"
  let thePort = options.port & fromMaybe 1234
  putText $ "Starting server on http://localhost:" <> show thePort
  run thePort $ logStdoutDev $ webDavServer (T.unpack options.dbPath)
