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
  Num (fromInteger),
  Traversable (traverse),
  concat,
  concatMap,
  elem,
  filter,
  fromIntegral,
  fromMaybe,
  fst,
  headMay,
  intercalate,
  mapM,
  mempty,
  null,
  pure,
  readMaybe,
  sequence,
  show,
  snd,
  zip,
  ($),
  (&&),
  (++),
  (-),
  (.),
  (/=),
  (<>),
  (==),
  (||),
 )

import Control.Exception (throw)
import Control.Monad (replicateM, when)
import Control.Monad.Catch (catchAll)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (Maybe (..), catMaybes, mapMaybe)
import Data.Text (Text, toLower)
import Data.Text qualified as T
import Data.Time (FormatTime, defaultTimeLocale, formatTime)
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
import Debug.Trace (traceM)
import Network.HTTP.Types.URI (urlDecode)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant (
  Application,
  Handler,
  NoContent (NoContent),
  Server,
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
  getModificationTime,
  listDirectory,
  removePathForcibly,
  renamePath,
 )
import System.FilePath (dropExtension)
import Text.XML.Light (
  Content (Elem),
  Element (elContent, elName),
  QName (qName),
 )

import SQLiteDAV.API (WebDavAPI, WithContentType (..), webDavAPI)
import SQLiteDAV.Properties (
  ItemType (File, Folder),
  PropResults (PropResults, itemType, propMissing, propName, props),
 )
import SQLiteDAV.Utils (formatTimestamp, sqlDataToText)


type String = [Char]


server :: FilePath -> Server WebDavAPI
server dbPath =
  doMkCol
    :<|> doPropFind dbPath
    :<|> doGet dbPath
    :<|> doPut
    :<|> doDelete
    :<|> doMove
    :<|> doCopy
    :<|> doOptions


webDavServer :: FilePath -> Application
webDavServer dbPath =
  addHeaders [("Dav", "1, 2, ordered-collections")]
    $ serve webDavAPI (server dbPath)


doOptions :: [String] -> Handler NoContent
doOptions urlPath = do
  pure NoContent


doMove :: [String] -> Maybe String -> Handler NoContent
doMove urlPath destinationMb = do
  traceM $ show urlPath ++ " moved to " ++ show destinationMb
  pure NoContent


doCopy :: [String] -> Maybe String -> Handler NoContent
doCopy urlPath destinationMb = do
  traceM $ show urlPath ++ " copied to " ++ show destinationMb
  pure NoContent


doPut :: [String] -> ByteString -> Handler NoContent
doPut urlPath body = do
  traceM $ "put " ++ show body ++ " into " ++ show urlPath
  pure NoContent


dataToContentType :: SQLData -> BL.ByteString
dataToContentType sqlData =
  case sqlData of
    SQLText _ -> "text/plain"
    SQLInteger _ -> "text/plain"
    SQLFloat _ -> "text/plain"
    SQLBlob _ -> "image/png"
    SQLNull -> "text/plain"


dataToFileExt :: SQLData -> String
dataToFileExt sqlData =
  case sqlData of
    SQLText _ -> ".txt"
    SQLInteger _ -> ".txt"
    SQLFloat _ -> ".txt"
    SQLBlob _ -> ""
    SQLNull -> ".txt"


doGet :: FilePath -> [String] -> Handler WithContentType
doGet dbPath urlPath = do
  case urlPath of
    tableName : rowidStr : colNameWithExt : _rest ->
      case readMaybe rowidStr of
        Nothing ->
          throwError err400{errBody = "Invalid rowid"}
        Just (rowid :: Integer) -> do
          colResult <- liftIO $ withConnection dbPath $ \conn -> do
            let
              colName = dropExtension colNameWithExt
              sqlQuery =
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
              pure
                $ WithContentType
                  { header = dataToContentType colData
                  , content = colData
                  }
            _ ->
              throwError
                err400
                  { errBody = "Multiple rows with the same rowid exist"
                  }
    _ ->
      throwError err404


getCellSize :: FilePath -> [String] -> Handler Integer
getCellSize dbPath urlPath = do
  case urlPath of
    tableName : rowidStr : colNameWithExt : _rest ->
      case readMaybe rowidStr of
        Nothing ->
          throwError err400{errBody = "Invalid rowid"}
        Just (rowid :: Integer) -> do
          colResult <- liftIO $ withConnection dbPath $ \conn -> do
            let
              colName = dropExtension colNameWithExt
              sqlQuery =
                Query
                  $ "SELECT length("
                  <> quoteKeyword (T.pack colName)
                  <> ") FROM "
                  <> quoteKeyword (T.pack tableName)
                  <> " WHERE rowid == ?"
            query conn sqlQuery (Only rowid)

          case colResult :: [Only SQLData] of
            [] ->
              throwError err404{errBody = "Row not found"}
            [Only colData] ->
              case colData of
                SQLInteger size ->
                  pure $ fromIntegral size
                _ ->
                  throwError err400{errBody = "Column is not an integer"}
            _ ->
              throwError
                err400
                  { errBody = "Multiple rows with the same rowid exist"
                  }
    _ ->
      pure 0


doDelete :: [String] -> Handler NoContent
doDelete urlPath = do
  traceM $ show urlPath ++ " deleted"
  pure NoContent


doMkCol :: [String] -> Handler NoContent
doMkCol urlPath = do
  traceM $ show urlPath ++ " collection created"
  pure NoContent


propNameToEntry
  :: FilePath
  -> [String]
  -> ItemType
  -> String
  -> Handler (Maybe (String, String))
propNameToEntry dbPath urlPath itemType propName = do
  let urlPathStr = "/" ++ intercalate "/" urlPath

  ( case propName of
      "creationdate" -> pure Nothing -- Unix doesn't store creation date
      "getcontentlength" ->
        if itemType == File
          then getCellSize dbPath urlPath <&> Just . show
          else pure Nothing
      "getlastmodified" -> do
        lastModified <- liftIO $ getModificationTime dbPath
        pure $ Just $ formatTimestamp lastModified
      -- "resourcetype" -- Handled by itemType
      _ -> pure Nothing
    )
    <&> (\valIO -> valIO <&> (propName,) . T.unpack)


keepMissingNames :: ItemType -> [String] -> [String]
keepMissingNames itemType propNames =
  let disallowed =
        [ "creationdate"
        , "quota-available-bytes"
        , "quota-used-bytes"
        , "quota"
        , "quotaused"
        ]
          ++ ( case itemType of
                File -> []
                Folder -> ["getcontentlength"]
             )
  in  propNames & filter (`elem` disallowed)


-- | Get the rows of a table as a list of lists of (col_name, SQLData) pairs
getTableRows :: FilePath -> Text -> IO [[(Text, SQLData)]]
getTableRows dbPath tableName =
  catchAll
    ( withConnection dbPath $ \conn -> do
        let sqlQuery = Query $ "SELECT rowid, * FROM " <> quoteKeyword tableName

        columns <- withStatement conn sqlQuery $ \stmt -> do
          numCols <- columnCount stmt
          let colNums = [0 .. (numCols - 1)]
          colNums & traverse (columnName stmt)

        tableRows :: [[SQLData]] <- query_ conn sqlQuery

        pure $ tableRows <&> zip columns
    )
    (\_ -> pure [])


getRowColumns :: FilePath -> Text -> Integer -> IO [(Text, SQLData)]
getRowColumns dbPath tableName rowid =
  catchAll
    ( withConnection dbPath $ \conn -> do
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
    )
    (\_ -> pure [])


ignoreHiddenFiles :: String -> Handler ()
ignoreHiddenFiles resourceName =
  when
    ( (".git" `isPrefixOf` resourceName)
        || (".hidden" `isPrefixOf` resourceName)
        || (".metadata_never_index" `isPrefixOf` resourceName)
        || (".ql_disablethumbnails" `isPrefixOf` resourceName)
        || (".Spotlight-V100" `isPrefixOf` resourceName)
        || ("._" `isPrefixOf` resourceName)
    )
    $ throwError err404


getPropsForTable
  :: FilePath
  -> [String]
  -> Maybe Text
  -> [String]
  -> String
  -> Handler [PropResults]
getPropsForTable dbPath urlPath depth propNames tableName = do
  props <-
    propNames
      & mapM (propNameToEntry dbPath urlPath Folder)
      <&> catMaybes

  let
    rootPropResult =
      PropResults
        { propName = tableName
        , itemType = Folder
        , props
        , propMissing = propNames & keepMissingNames Folder
        }
    depthLow = depth <&> toLower

  ignoreHiddenFiles tableName

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
                  , props
                  , propMissing = propNames & keepMissingNames Folder
                  }
            )

  pure
    $ rootPropResult
    : if depthLow /= Just "1" && depthLow /= Just "infinity"
      then []
      else ioTableRows


getPropsForRow
  :: FilePath
  -> [String]
  -> Maybe Text
  -> [String]
  -> String
  -> Maybe Integer
  -> Handler [PropResults]
getPropsForRow dbPath urlPath depth propNames tableName rowidMb = do
  rootProps <-
    propNames
      & mapM (propNameToEntry dbPath urlPath Folder)
      <&> catMaybes

  let
    rowid = case rowidMb of
      Nothing -> throw $ err404{errBody = "Row not found"}
      Just rowidInteger -> rowidInteger

    rootPropResult =
      PropResults
        { propName = tableName <> "/" <> show rowid
        , itemType = Folder
        , props = rootProps
        , propMissing = propNames & keepMissingNames Folder
        }
    depthLow = depth <&> toLower

  rowColumns <- liftIO $ getRowColumns dbPath (T.pack tableName) rowid

  when (null rowColumns)
    $ throwError
      err404{errBody = "Row does not exist or does not have any columns"}

  propResults <-
    mapM
      ( \rowColumn -> do
          let
            colName = T.unpack (fst rowColumn)
            getPropName tableName =
              tableName
                ++ "/"
                ++ show rowid
                ++ "/"
                ++ colName
                ++ dataToFileExt (snd rowColumn)

          props <-
            propNames
              & mapM (propNameToEntry dbPath (urlPath ++ [colName]) File)
              <&> catMaybes

          pure
            $ PropResults
              { propName = getPropName tableName
              , itemType = File
              , props
              , propMissing = propNames & keepMissingNames File
              }
      )
      rowColumns

  pure
    $ rootPropResult
    : if depthLow /= Just "1" && depthLow /= Just "infinity"
      then []
      else propResults


getPropsForCell
  :: FilePath
  -> [String]
  -> [String]
  -> String
  -> Maybe Integer
  -> String
  -> Handler [PropResults]
getPropsForCell dbPath urlPath propNames tableName rowidMb colNameWithExt = do
  props <-
    propNames
      & mapM (propNameToEntry dbPath urlPath File)
      <&> catMaybes

  let
    colName = dropExtension colNameWithExt
    rowid = case rowidMb of
      Nothing -> throw $ err404{errBody = "Row not found"}
      Just rowidInteger -> rowidInteger

    rootPropResult =
      PropResults
        { propName = tableName <> "/" <> show rowid <> "/" <> colName
        , itemType = File
        , props
        , propMissing = propNames & keepMissingNames File
        }

  ignoreHiddenFiles colName

  pure [rootPropResult]


doPropFind
  :: String
  -> [String]
  -> Maybe Text
  -> Element
  -> Handler [PropResults]
doPropFind dbPath urlPath depth doc = do
  let
    urlPathNorm = urlPath & filter (/= "")
    propNames =
      [ qName $ elName x
      | Elem x <- concatMap elContent ([x | Elem x <- elContent doc])
      ]

  case urlPathNorm of
    [] -> do
      let itemType = Folder

      itemProps <-
        propNames
          & mapM (propNameToEntry dbPath urlPathNorm itemType)
          <&> catMaybes

      let
        rootPropResult =
          PropResults
            { propName = ""
            , itemType
            , props = itemProps
            , propMissing = propNames & keepMissingNames itemType
            }
        depthLow = depth <&> toLower

      (tableRows :: [[Text]]) <-
        liftIO $ withConnection dbPath $ \conn ->
          query_ conn "SELECT name FROM sqlite_master WHERE type == 'table'"

      folderProps <-
        propNames
          & mapM (propNameToEntry dbPath urlPathNorm Folder)
          <&> catMaybes

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
                  , props = folderProps
                  , propMissing = propNames & keepMissingNames Folder
                  }
    --
    [tableName] ->
      getPropsForTable dbPath urlPathNorm depth propNames tableName
    --
    [tableName, rowName] ->
      getPropsForRow
        dbPath
        urlPathNorm
        depth
        propNames
        tableName
        (readMaybe rowName)
    --
    tableName : rowName : colNameWithExt : _rest ->
      getPropsForCell
        dbPath
        urlPathNorm
        propNames
        tableName
        (readMaybe rowName)
        colNameWithExt


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
