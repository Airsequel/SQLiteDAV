{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use unless" #-}

module Network.WebDav.Server where

import Protolude (
  Char,
  IO,
  Maybe,
  Traversable (traverse),
  concat,
  concatMap,
  elem,
  filter,
  fromMaybe,
  headMay,
  pure,
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
import Database.SQLite.Simple.Types (Only)
import Debug.Trace (traceM, traceShowId, traceShowM)
import Network.HTTP.Types.URI (urlDecode)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Network.WebDav.API (webDavAPI)
import Network.WebDav.Constants (dbPath, webBase)
import Network.WebDav.Properties (
  ItemType (File, Folder),
  PropResults (PropResults, itemType, propMissing, propName, props),
 )
import Servant (
  Application,
  Handler,
  err404,
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


type String = [Char]


webDavServer :: Application
webDavServer =
  addHeaders [("Dav", "1, 2, ordered-collections")] $
    provideOptions webDavAPI $
      serve
        webDavAPI
        ( doMkCol
            :<|> doPropFind
            :<|> doGet
            :<|> doPut
            :<|> doDelete
            :<|> doMove
            :<|> doCopy
            :<|> doOptions
        )


doOptions :: [String] -> Handler ()
doOptions _ = do
  pure ()


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


doGet :: [String] -> Handler String
doGet urlPath =
  pure "FILE CONTENT"


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


sqlDataToText :: SQLData -> Text
sqlDataToText sqlData =
  case sqlData of
    SQLText text -> text
    SQLInteger int -> T.pack $ show int
    SQLFloat float -> T.pack $ show float
    SQLBlob blob -> T.pack $ show blob
    SQLNull -> "NULL"


-- | Get the rows of a table as a list of lists of (col_name, SQLData) pairs
getTableRows :: Text -> IO [[(Text, SQLData)]]
getTableRows tableName =
  withConnection "test/data.sqlite" $ \conn -> do
    let sqlQuery = Query $ "SELECT rowid, * FROM " <> tableName

    columns <- withStatement conn sqlQuery $ \stmt -> do
      numCols <- columnCount stmt
      let colNums = [0 .. (numCols - 1)]
      colNums & traverse (columnName stmt)

    tableRows :: [[SQLData]] <- query_ conn sqlQuery

    pure $ tableRows <&> zip columns


getPropsForTable :: Maybe Text -> [String] -> String -> Handler [PropResults]
getPropsForTable depth propNames tableName = do
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
    ( "._" `isPrefixOf` tableName
        || ".metadata_never_index" `isPrefixOf` tableName
        || ".Spotlight-V100" `isPrefixOf` tableName
        || ".hidden" `isPrefixOf` tableName
    )
    $ throwError err404

  traceShowM depthLow

  tableRows <- liftIO $ getTableRows (quoteKeyword (T.pack tableName))

  traceShowM tableRows

  let ioTableRows =
        tableRows
          <&> ( \tableRow ->
                  PropResults
                    { propName =
                        tableName
                          ++ "/"
                          ++ ( tableRow
                                & headMay
                                & fromMaybe ("ERROR", SQLText "ERROR")
                                & snd
                                & sqlDataToText
                                & T.unpack
                             )
                    , itemType = Folder
                    , props = propNames & mapMaybe (propNameToEntry Folder)
                    , propMissing = propNames & keepMissingNames Folder
                    }
              )

  pure $
    rootPropResult
      : if depthLow /= Just "1" && depthLow /= Just "infinity"
        then []
        else ioTableRows


doPropFind :: [String] -> Maybe Text -> Element -> Handler [PropResults]
doPropFind urlPath depth doc = do
  let propNames =
        [ qName $ elName x
        | Elem x <- concatMap elContent ([x | Elem x <- elContent doc])
        ]

  traceM $
    "doPropFind:"
      ++ ("\nurlPath: " ++ show urlPath)
      ++ ("\npropNames: " ++ show propNames)

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
        liftIO $ withConnection "test/data.sqlite" $ \conn ->
          query_ conn "SELECT name FROM sqlite_master WHERE type == 'table'"

      pure $
        traceShowId $
          rootPropResult
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
      getPropsForTable depth propNames tableName
    --
    [tableName, ""] ->
      -- TODO
      pure []
    --
    [tableName, rowName] ->
      -- TODO
      pure []


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
