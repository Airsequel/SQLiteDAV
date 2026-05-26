{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Protolude (
  IO,
  Int,
  Maybe (..),
  Text,
  die,
  fromMaybe,
  pure,
  putText,
  show,
  ($),
  (&),
  (<$>),
  (<*>),
  (<>),
 )

import Data.Text qualified as T
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Applicative (
  Parser,
  argument,
  auto,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  progDesc,
  str,
  strOption,
  (<**>),
 )

import SQLiteDAV.Server (RowNameMode (..), parseRowNameMode, webDavServer)


data Options = Options
  { port :: Maybe Int
  , rowName :: Maybe Text
  , dbPath :: Text
  }


optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( option
          auto
          ( long "port"
              <> metavar "INT"
              <> help "Port to listen on (default: 1234)"
          )
      )
    <*> optional
      ( strOption
          ( long "rowname"
              <> metavar "TEXT"
              <> help
                "How to name row directories: \
                \rowid (default), pk, or combined"
          )
      )
    <*> argument str (metavar "DB_PATH" <> help "Path to SQLite database file")


main :: IO ()
main = do
  options <-
    execParser $
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "SQLiteDAV server")
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
