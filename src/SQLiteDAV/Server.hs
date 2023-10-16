{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use unless" #-}

module SQLiteDAV.Server where

import Protolude (
  Char,
  FilePath,
  IO,
  Integer,
  Maybe,
  Traversable (traverse),
  concat,
  concatMap,
  elem,
  filter,
  fromMaybe,
  fst,
  headMay,
  mempty,
  pure,
  readMaybe,
  show,
  snd,
  zip,
  ($),
  (&&),
  (++),
  (-),
  (/=),
  (<>),
  (==),
  (||),
 )

import Control.Exception (throw)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (Maybe (..), catMaybes, mapMaybe)
import Data.Text (Text, toLower)
import Data.Text qualified as T
import Data.Traversable (for)
import Database.SQLite.Simple (
  Query (Query),
  SQLData (..),
  columnCount,
  columnName,
  query,
  query_,
  withConnection,
  withStatement,
 )
import Database.SQLite.Simple.Types (Only (Only))
import Debug.Trace (traceM, traceShowId, traceShowM)
import Network.HTTP.Types.URI (urlDecode)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import SQLiteDAV.API (webDavAPI)
import Servant (
  Application,
  Handler,
  NoContent (NoContent),
  err400,
  err404,
  errBody,
  serve,
  throwError,
  (:<|>) ((:<|>)),
 )
import System.Directory (
  copyFile,
  createDirectory,
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
  removePathForcibly,
  renamePath,
 )
import Text.XML.Light (
  Content (Elem),
  Element (elContent, elName),
  QName (qName),
 )

import SQLiteDAV.Properties (
  ItemType (File, Folder),
  PropResults (PropResults, itemType, propMissing, propName, props),
 )
import SQLiteDAV.Utils (sqlDataToText)


type String = [Char]


webDavServer :: FilePath -> Application
webDavServer dbPath =
  addHeaders [("Dav", "1, 2, ordered-collections")]
    $ provideOptions webDavAPI
    $ serve
      webDavAPI
      ( doMkCol
          :<|> doPropFind dbPath
          :<|> doGet dbPath
          :<|> doPut
          :<|> doDelete
          :<|> doMove
          :<|> doCopy
          :<|> doOptions
      )


doOptions :: [String] -> Handler NoContent
doOptions _ =
  pure NoContent


doMove :: [String] -> Maybe String -> Handler ()
doMove urlPath destinationMb = do
  traceM $ show urlPath ++ " moved to " ++ show destinationMb
  pure ()


doCopy :: [String] -> Maybe String -> Handler ()
doCopy urlPath destinationMb = do
  traceM $ show urlPath ++ " copied to " ++ show destinationMb
  pure ()


doPut :: [String] -> ByteString -> Handler ()
doPut urlPath body = do
  traceM $ "put " ++ show body ++ " into " ++ show urlPath
  pure ()


doGet :: FilePath -> [String] -> Handler SQLData
doGet dbPath urlPath = do
  case urlPath of
    tableName : rowidStr : colName : _rest ->
      case readMaybe rowidStr of
        Nothing ->
          throwError err400{errBody = "Invalid rowid"}
        Just (rowid :: Integer) -> do
          colResult <- liftIO $ withConnection dbPath $ \conn -> do
            let sqlQuery =
                  Query
                    $ "SELECT "
                    <> quoteKeyword (T.pack colName)
                    <> " FROM "
                    <> quoteKeyword (T.pack tableName)
                    <> " WHERE rowid == ?"
            query conn sqlQuery (Only rowid)

          case colResult :: [Only SQLData] of
            [] ->
              throwError err404{errBody = "Row not found"}
            [Only colData] ->
              pure colData
            _ ->
              throwError
                err400
                  { errBody = "Multiple rows with the same rowid exist"
                  }
    _ ->
      throwError err404


doDelete :: [String] -> Handler ()
doDelete urlPath = do
  traceM $ show urlPath ++ " deleted"
  pure ()


doMkCol :: [String] -> Handler ()
doMkCol urlPath = do
  traceM $ show urlPath ++ " collection created"
  pure ()


propNameToEntry :: ItemType -> String -> Maybe (String, String)
propNameToEntry itemType propName =
  ( case propName of
      "creationdate" -> Just "2021-01-01T00:00:00Z"
      "getcontentlength" ->
        if itemType == File
          then Just "1000"
          else Nothing
      "getlastmodified" -> Just "Fri, 13 Oct 2020 18:35:34 GMT"
      "quota-available-bytes" -> Just "1000000"
      "quota-used-bytes" -> Just "1000"
      -- "resourcetype" -- Handled by itemType
      _ -> Nothing
  )
    <&> (propName,)


keepMissingNames :: ItemType -> [String] -> [String]
keepMissingNames itemType propNames =
  let disallowed =
        ["quota", "quotaused"]
          ++ ( case itemType of
                File -> []
                Folder -> ["getcontentlength"]
             )
  in  propNames & filter (`elem` disallowed)


-- | Get the rows of a table as a list of lists of (col_name, SQLData) pairs
getTableRows :: FilePath -> Text -> IO [[(Text, SQLData)]]
getTableRows dbPath tableName =
  withConnection dbPath $ \conn -> do
    let sqlQuery = Query $ "SELECT rowid, * FROM " <> quoteKeyword tableName

    columns <- withStatement conn sqlQuery $ \stmt -> do
      numCols <- columnCount stmt
      let colNums = [0 .. (numCols - 1)]
      colNums & traverse (columnName stmt)

    tableRows :: [[SQLData]] <- query_ conn sqlQuery

    pure $ tableRows <&> zip columns


getRowColumns :: FilePath -> Text -> Integer -> IO [(Text, SQLData)]
getRowColumns dbPath tableName rowid =
  withConnection dbPath $ \conn -> do
    let sqlQuery =
          Query
            $ "SELECT * FROM "
            <> quoteKeyword tableName
            <> " WHERE rowid = ?"

    columns <- withStatement conn sqlQuery $ \stmt -> do
      numCols <- columnCount stmt
      let colNums = [0 .. (numCols - 1)]
      colNums & traverse (columnName stmt)

    tableRows :: [[SQLData]] <- query conn sqlQuery (Only rowid)

    case tableRows of
      [] -> pure []
      [row] -> pure $ row & zip columns


getPropsForTable :: FilePath ->
   Maybe Text -> [String] -> String -> Handler [PropResults]
getPropsForTable dbPath depth propNames tableName = do
  let
    rootPropResult =
      PropResults
        { propName = tableName
        , itemType = Folder
        , props = propNames & mapMaybe (propNameToEntry Folder)
        , propMissing = propNames & keepMissingNames Folder
        }
    depthLow = depth <&> toLower

  when
    ( ("._" `isPrefixOf` tableName)
        || (".metadata_never_index" `isPrefixOf` tableName)
        || (".Spotlight-V100" `isPrefixOf` tableName)
        || (".hidden" `isPrefixOf` tableName)
    )
    $ throwError err404

  tableRows <- liftIO $ getTableRows dbPath (T.pack tableName)

  let
    getPropName tableRow =
      tableName
        ++ "/"
        ++ ( tableRow
              & headMay
              & fromMaybe ("ERROR", SQLText "ERROR")
              & snd
              & sqlDataToText
              & T.unpack
           )
    ioTableRows =
      tableRows
        <&> ( \tableRow ->
                PropResults
                  { propName = getPropName tableRow
                  , itemType = Folder
                  , props = propNames & mapMaybe (propNameToEntry Folder)
                  , propMissing = propNames & keepMissingNames Folder
                  }
            )

  pure
    $ rootPropResult
    : if depthLow /= Just "1" && depthLow /= Just "infinity"
      then []
      else ioTableRows


getPropsForRow
  :: FilePath -> Maybe Text
  -> [String]
  -> String
  -> Maybe Integer
  -> Handler [PropResults]
getPropsForRow dbPath depth propNames tableName rowidMb = do
  let
    rowid = case rowidMb of
      Nothing -> throw $ err404{errBody = "Row not found"}
      Just rowidInteger -> rowidInteger

    rootPropResult =
      PropResults
        { propName = tableName <> "/" <> show rowid
        , itemType = Folder
        , props = propNames & mapMaybe (propNameToEntry Folder)
        , propMissing = propNames & keepMissingNames Folder
        }
    depthLow = depth <&> toLower

  rowColumns <- liftIO $ getRowColumns dbPath (T.pack tableName) rowid

  let
    getPropName tableName colName =
      tableName ++ "/" ++ show rowid ++ "/" ++ colName
    ioRowColumns =
      rowColumns
        <&> ( \rowColumn ->
                PropResults
                  { propName = getPropName tableName (T.unpack $ fst rowColumn)
                  , itemType = File
                  , props = propNames & mapMaybe (propNameToEntry File)
                  , propMissing = propNames & keepMissingNames File
                  }
            )

  pure
    $ rootPropResult
    : if depthLow /= Just "1" && depthLow /= Just "infinity"
      then []
      else ioRowColumns


doPropFind
  :: String
  -> [String]
  -> Maybe Text
  -> Element
  -> Handler [PropResults]
doPropFind dbPath urlPath depth doc = do
  let propNames =
        [ qName $ elName x
        | Elem x <- concatMap elContent ([x | Elem x <- elContent doc])
        ]

  case urlPath of
    [] -> do
      let
        itemType = Folder
        rootPropResult =
          PropResults
            { propName = ""
            , itemType
            , props = propNames & mapMaybe (propNameToEntry itemType)
            , propMissing = propNames & keepMissingNames itemType
            }
        depthLow = depth <&> toLower

      (tableRows :: [[Text]]) <-
        liftIO $ withConnection dbPath $ \conn ->
          query_ conn "SELECT name FROM sqlite_master WHERE type == 'table'"

      pure
        $ rootPropResult
        : if depthLow /= Just "1" && depthLow /= Just "infinity"
          then []
          else
            tableRows
              & concat
              <&> \table ->
                PropResults
                  { propName = T.unpack table
                  , itemType = Folder
                  , props = propNames & mapMaybe (propNameToEntry Folder)
                  , propMissing = propNames & keepMissingNames Folder
                  }
    --
    [tableName] ->
      getPropsForTable dbPath depth propNames tableName
    --
    [tableName, ""] ->
      getPropsForTable dbPath depth propNames tableName
    --
    tableName : rowName : rest ->
      getPropsForRow dbPath depth propNames tableName (readMaybe rowName)


-- | Escape double quotes in SQL strings
escDoubleQuotes :: Text -> Text
escDoubleQuotes =
  T.replace "\"" "\"\""


-- | Quote a keyword in an SQL query
quoteKeyword :: Text -> Text
quoteKeyword keyword =
  keyword
    & escDoubleQuotes
    & (\word -> "\"" <> word <> "\"")

-- TODO: Support for `Prefer: depth-noroot` (https://www.rfc-editor.org/rfc/rfc8144.txt)
-- TODO: No content response for options
-- TODO: Only allow rowid tables, or tables with one primary key
