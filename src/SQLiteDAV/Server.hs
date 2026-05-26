{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use unless" #-}

module SQLiteDAV.Server where

import Protolude (
  Bool (False, True),
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
  fmap,
  fromIntegral,
  fromMaybe,
  fst,
  headMay,
  intercalate,
  lastMay,
  mapM,
  mempty,
  not,
  null,
  otherwise,
  pure,
  readMaybe,
  sequence,
  show,
  snd,
  truncate,
  zip,
  ($),
  (&&),
  (++),
  (-),
  (.),
  (/=),
  (<>),
  (==),
  (>),
  (||),
 )

import Control.Exception (throw)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Catch (catchAll)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (Maybe (..), catMaybes, isJust, isNothing, mapMaybe)
import Data.Text (Text, toLower)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (FormatTime, UTCTime, defaultTimeLocale, formatTime)
import Data.Traversable (for)
import Database.SQLite.Simple (
  Query (Query),
  SQLData (..),
  columnCount,
  columnName,
  execute,
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
  Header,
  Headers,
  NoContent (NoContent),
  Server,
  ServerError (..),
  addHeader,
  err400,
  err403,
  err404,
  err405,
  err409,
  err412,
  err415,
  err502,
  errBody,
  noHeader,
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
import SQLiteDAV.MimeDetect (detectMimeType, extensionForMime)
import SQLiteDAV.Properties (
  ItemType (File, Folder),
  LockResult (LockResult, lockRoot, lockToken),
  PropResults (PropResults, itemType, propMissing, propName, props),
 )
import SQLiteDAV.SQLAR qualified as SQLAR
import SQLiteDAV.Utils (formatTimestamp, sqlDataToText)

import Data.Time.Clock.POSIX (getPOSIXTime)


type String = [Char]


server :: FilePath -> Server WebDavAPI
server dbPath =
  doMkCol dbPath
    :<|> doPropFind dbPath
    :<|> doGet dbPath
    :<|> doPut dbPath
    :<|> doDelete dbPath
    :<|> doMove dbPath
    :<|> doCopy dbPath
    :<|> doLock
    :<|> doUnlock
    :<|> doOptions


webDavServer :: FilePath -> Application
webDavServer dbPath =
  addHeaders [("Dav", "1, 2, ordered-collections")] $
    serve webDavAPI (server dbPath)


doOptions :: [String] -> Handler NoContent
doOptions urlPath = do
  pure NoContent


{-| Strip an optional @scheme://host[:port]@ prefix and split the
remaining path into URL-decoded segments.
-}
parseDestination :: String -> Maybe [String]
parseDestination raw =
  let
    d = T.pack raw
    afterScheme
      | "http://" `T.isPrefixOf` d = T.drop 7 d
      | "https://" `T.isPrefixOf` d = T.drop 8 d
      | otherwise = d
    -- After scheme: "host:port/path/..." or just "/path/...".
    pathOnly =
      if T.null afterScheme || T.head afterScheme == '/'
        then afterScheme
        else T.dropWhile (/= '/') afterScheme
    decoded =
      T.decodeUtf8 (urlDecode False (T.encodeUtf8 pathOnly))
    segments =
      decoded
        & T.splitOn "/"
        & fmap T.unpack
        & filter (not . null)
  in
    Just segments


{-| RFC 4918 mandates 204 No Content when COPY/MOVE replaces an
existing resource. Servant's verb-derived status applies only when
the handler returns normally, so we 'throwError' a success-shaped
ServerError to override it.
-}
overwroteResponse :: ServerError
overwroteResponse =
  ServerError
    { errHTTPCode = 204
    , errReasonPhrase = "No Content"
    , errBody = ""
    , errHeaders = []
    }


{-| True when an Overwrite header instructs the server to overwrite.
Default is overwrite per RFC 4918 §10.6.
-}
overwriteAllowed :: Maybe String -> Bool
overwriteAllowed Nothing = True
overwriteAllowed (Just hdr) =
  case [c | c <- hdr, not (isSpace c)] of
    "F" -> False
    "f" -> False
    _ -> True


-- | True when a Depth header asks for a shallow operation (Depth: 0).
isDepthZero :: Maybe String -> Bool
isDepthZero Nothing = False
isDepthZero (Just hdr) =
  [c | c <- hdr, not (isSpace c)] == "0"


{-| Resolve the destination header against the source request and
return the table name and archive path on success. Fails with the
appropriate WebDAV error code if the destination is malformed,
points into a different sqlar table, or its parent does not exist.
-}
resolveSqlarDestination ::
  FilePath ->
  String ->
  Maybe String ->
  Handler (String, Text)
resolveSqlarDestination dbPath srcTable destinationMb = do
  rawDest <- case destinationMb of
    Nothing -> throwError err400{errBody = "Missing Destination header"}
    Just d -> pure d
  dstSegments <- case parseDestination rawDest of
    Just segs@(_ : _) -> pure segs
    _ -> throwError err400{errBody = "Invalid Destination header"}
  let
    dstTable : dstRest = dstSegments
  -- Cross-archive operations are not supported (RFC 4918 §9.9.4
  -- suggests 502 Bad Gateway).
  when (dstTable /= srcTable) $
    throwError err502{errBody = "Cross-archive COPY/MOVE not supported"}
  let dstArchive = SQLAR.archivePath dstRest
  -- Empty dst path means moving onto the archive root, which is not
  -- meaningful here.
  when (T.null dstArchive) $
    throwError err403{errBody = "Cannot operate on archive root"}
  parentOk <-
    liftIO $ SQLAR.parentExists dbPath (T.pack dstTable) dstArchive
  unless parentOk $
    throwError err409{errBody = "Destination parent does not exist"}
  pure (dstTable, dstArchive)


doMove ::
  FilePath ->
  [String] ->
  Maybe String ->
  Maybe String ->
  Handler NoContent
doMove dbPath urlPath destinationMb overwriteMb = do
  let urlPathNorm = urlPath & filter (/= "")
  case urlPathNorm of
    tableName : rest@(_ : _) -> do
      isSqlar <- liftIO $ SQLAR.isSqlarTable dbPath (T.pack tableName)
      if isSqlar
        then doMoveSqlar dbPath tableName rest destinationMb overwriteMb
        else do
          traceM $ show urlPath ++ " moved to " ++ show destinationMb
          pure NoContent
    _ ->
      throwError err404{errBody = "Source not found"}


doMoveSqlar ::
  FilePath ->
  String ->
  [String] ->
  Maybe String ->
  Maybe String ->
  Handler NoContent
doMoveSqlar dbPath tableName rest destinationMb overwriteMb = do
  let srcArchive = SQLAR.archivePath rest
  srcEntry <-
    liftIO $ SQLAR.resolvePath dbPath (T.pack tableName) srcArchive
  when (isNothing srcEntry) $
    throwError err404{errBody = "Source not found"}
  (_, dstArchive) <- resolveSqlarDestination dbPath tableName destinationMb
  -- 412 when the destination exists and Overwrite: F.
  dstExists <- liftIO $ SQLAR.hasPath dbPath (T.pack tableName) dstArchive
  when (dstExists && not (overwriteAllowed overwriteMb)) $
    throwError err412{errBody = "Destination exists and Overwrite: F"}
  -- Per RFC 4918 §9.9.3, MOVE always behaves as Depth: infinity, and an
  -- overwrite replaces any prior subtree at the destination.
  when dstExists $
    liftIO $
      SQLAR.deleteSubtree dbPath (T.pack tableName) dstArchive
  liftIO $ SQLAR.copySubtree dbPath (T.pack tableName) srcArchive dstArchive
  liftIO $ SQLAR.deleteSubtree dbPath (T.pack tableName) srcArchive
  when dstExists $ throwError overwroteResponse
  pure NoContent


doCopy ::
  FilePath ->
  [String] ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Handler NoContent
doCopy dbPath urlPath destinationMb overwriteMb depthMb = do
  let urlPathNorm = urlPath & filter (/= "")
  case urlPathNorm of
    tableName : rest@(_ : _) -> do
      isSqlar <- liftIO $ SQLAR.isSqlarTable dbPath (T.pack tableName)
      if isSqlar
        then
          doCopySqlar
            dbPath
            tableName
            rest
            destinationMb
            overwriteMb
            depthMb
        else do
          traceM $ show urlPath ++ " copied to " ++ show destinationMb
          pure NoContent
    _ ->
      throwError err404{errBody = "Source not found"}


doCopySqlar ::
  FilePath ->
  String ->
  [String] ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Handler NoContent
doCopySqlar dbPath tableName rest destinationMb overwriteMb depthMb = do
  let srcArchive = SQLAR.archivePath rest
  srcEntry <-
    liftIO $ SQLAR.resolvePath dbPath (T.pack tableName) srcArchive
  entry <- case srcEntry of
    Nothing -> throwError err404{errBody = "Source not found"}
    Just e -> pure e
  (_, dstArchive) <- resolveSqlarDestination dbPath tableName destinationMb
  dstExists <- liftIO $ SQLAR.hasPath dbPath (T.pack tableName) dstArchive
  when (dstExists && not (overwriteAllowed overwriteMb)) $
    throwError err412{errBody = "Destination exists and Overwrite: F"}
  when dstExists $
    liftIO $
      SQLAR.deleteSubtree dbPath (T.pack tableName) dstArchive
  case SQLAR.entryType entry of
    Folder | isDepthZero depthMb -> do
      -- RFC 4918 §9.8.3: Depth: 0 on a collection copies the collection
      -- itself, not its members. Create a fresh empty folder entry.
      now <- liftIO getPOSIXTime
      liftIO $
        SQLAR.insertEntry
          dbPath
          (T.pack tableName)
          dstArchive
          0o040755
          (truncate now)
          ByteString.empty
    _ ->
      liftIO $
        SQLAR.copySubtree dbPath (T.pack tableName) srcArchive dstArchive
  when dstExists $ throwError overwroteResponse
  pure NoContent


-- Locks are not tracked; the fake token only satisfies clients
-- (e.g. macOS Finder) that require LOCK before issuing a PUT.
fakeLockToken :: String
fakeLockToken = "urn:uuid:00000000-0000-0000-0000-000000000001"


doLock ::
  [String] ->
  Handler (Headers '[Header "Lock-Token" String] LockResult)
doLock urlPath = do
  let
    root = "/" ++ intercalate "/" urlPath
    lockResult =
      LockResult{lockToken = fakeLockToken, lockRoot = root}
  pure $ addHeader ("<" ++ fakeLockToken ++ ">") lockResult


doUnlock :: [String] -> Maybe String -> Handler NoContent
doUnlock urlPath tokenMb = do
  traceM $ show urlPath ++ " unlocked with token " ++ show tokenMb
  pure NoContent


doPut :: FilePath -> [String] -> ByteString -> Handler NoContent
doPut dbPath urlPath body = do
  let urlPathNorm = urlPath & filter (/= "")
  case urlPathNorm of
    tableName : rest@(_ : _) -> do
      isSqlar <- liftIO $ SQLAR.isSqlarTable dbPath (T.pack tableName)
      if isSqlar
        then do
          let archive = SQLAR.archivePath rest
          -- RFC 4918 §9.7.1: PUT requires the parent collection to exist.
          parentOk <-
            liftIO $ SQLAR.parentExists dbPath (T.pack tableName) archive
          unless parentOk $
            throwError err409{errBody = "Parent collection does not exist"}
          -- A PUT onto an existing collection is not meaningful for a file.
          existing <-
            liftIO $ SQLAR.resolvePath dbPath (T.pack tableName) archive
          case existing of
            Just e
              | SQLAR.entryType e == Folder ->
                  throwError err405{errBody = "Path is a collection"}
            _ -> pure ()
          now <- liftIO getPOSIXTime
          let
            -- 0o100644 == regular file with 0644 perms
            mode :: Integer
            mode = 0o100644
          liftIO $
            SQLAR.insertEntry
              dbPath
              (T.pack tableName)
              archive
              (fromInteger mode)
              (truncate now)
              body
          pure NoContent
        else do
          traceM $ "put " ++ show body ++ " into " ++ show urlPath
          pure NoContent
    _ -> do
      traceM $ "put " ++ show body ++ " into " ++ show urlPath
      pure NoContent


dataToContentType :: SQLData -> IO BL.ByteString
dataToContentType sqlData =
  case sqlData of
    SQLText _ -> pure "text/plain"
    SQLInteger _ -> pure "text/plain"
    SQLFloat _ -> pure "text/plain"
    SQLBlob blob -> do
      mime <- detectMimeType blob
      pure $ BL.fromStrict $ T.encodeUtf8 mime
    SQLNull -> pure "text/plain"


dataToFileExt :: SQLData -> IO String
dataToFileExt sqlData =
  case sqlData of
    SQLText _ -> pure ".txt"
    SQLInteger _ -> pure ".txt"
    SQLFloat _ -> pure ".txt"
    SQLBlob blob -> do
      mime <- detectMimeType blob
      pure $ case extensionForMime mime of
        Just ext -> "." <> T.unpack ext
        Nothing -> ""
    SQLNull -> pure ".txt"


doGet :: FilePath -> [String] -> Handler WithContentType
doGet dbPath urlPath = do
  let urlPathNorm = urlPath & filter (/= "")
  case urlPathNorm of
    tableName : rest@(_ : _) -> do
      isSqlar <- liftIO $ SQLAR.isSqlarTable dbPath (T.pack tableName)
      if isSqlar
        then doGetSqlar dbPath tableName rest
        else case rest of
          rowidStr : colNameWithExt : _rest ->
            doGetCell dbPath tableName rowidStr colNameWithExt
          _ -> throwError err404
    _ ->
      throwError err404


doGetCell :: FilePath -> String -> String -> String -> Handler WithContentType
doGetCell dbPath tableName rowidStr colNameWithExt =
  case readMaybe rowidStr of
    Nothing ->
      throwError err400{errBody = "Invalid rowid"}
    Just (rowid :: Integer) -> do
      colResult <- liftIO $ withConnection dbPath $ \conn -> do
        let
          colName = dropExtension colNameWithExt
          sqlQuery =
            Query $
              "SELECT "
                <> quoteKeyword (T.pack colName)
                <> " FROM "
                <> quoteKeyword (T.pack tableName)
                <> " WHERE rowid == ?"
        query conn sqlQuery (Only rowid)

      case colResult :: [Only SQLData] of
        [] ->
          throwError err404{errBody = "Row not found"}
        [Only colData] -> do
          contentType <- liftIO $ dataToContentType colData
          pure $
            WithContentType
              { header = contentType
              , content = colData
              }
        _ ->
          throwError
            err400
              { errBody = "Multiple rows with the same rowid exist"
              }


doGetSqlar :: FilePath -> String -> [String] -> Handler WithContentType
doGetSqlar dbPath tableName rest = do
  let archive = SQLAR.archivePath rest
  fileMb <- liftIO $ SQLAR.lookupEntry dbPath (T.pack tableName) archive
  case fileMb of
    Nothing ->
      throwError err404{errBody = "File not found in archive"}
    Just file -> do
      mime <- liftIO $ detectMimeType (SQLAR.fileContent file)
      pure $
        WithContentType
          { header = BL.fromStrict (T.encodeUtf8 mime)
          , content = SQLBlob (SQLAR.fileContent file)
          }


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
                Query $
                  "SELECT length("
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


doDelete :: FilePath -> [String] -> Handler NoContent
doDelete dbPath urlPath = do
  let urlPathNorm = urlPath & filter (/= "")
  case urlPathNorm of
    tableName : rest@(_ : _) -> do
      isSqlar <- liftIO $ SQLAR.isSqlarTable dbPath (T.pack tableName)
      if isSqlar
        then do
          let archive = SQLAR.archivePath rest
          exists <- liftIO $ SQLAR.hasPath dbPath (T.pack tableName) archive
          unless exists $
            throwError err404{errBody = "Path not found in archive"}
          liftIO $ SQLAR.deleteSubtree dbPath (T.pack tableName) archive
          pure NoContent
        else case rest of
          rowidStr : colNameWithExt : _rest ->
            case readMaybe rowidStr of
              Nothing ->
                throwError err400{errBody = "Invalid rowid"}
              Just (rowid :: Integer) -> do
                let
                  colName = dropExtension colNameWithExt
                  sqlQuery =
                    Query $
                      "UPDATE "
                        <> quoteKeyword (T.pack tableName)
                        <> " SET "
                        <> quoteKeyword (T.pack colName)
                        <> " = NULL WHERE rowid == ?"
                liftIO $ withConnection dbPath $ \conn ->
                  execute conn sqlQuery (Only rowid)
                pure NoContent
          _ -> throwError err404
    _ ->
      throwError err404


doMkCol ::
  FilePath ->
  [String] ->
  Maybe Integer ->
  Handler NoContent
doMkCol dbPath urlPath contentLengthMb = do
  -- RFC 4918 §9.3: MKCOL with a non-empty body must be rejected with 415.
  -- We treat a positive Content-Length as the unambiguous signal of a body
  -- (clients without bodies send 0 or omit the header).
  case contentLengthMb of
    Just n
      | n > 0 ->
          throwError err415{errBody = "MKCOL must have an empty body"}
    _ -> pure ()
  let urlPathNorm = urlPath & filter (/= "")
  case urlPathNorm of
    tableName : rest@(_ : _) -> do
      isSqlar <- liftIO $ SQLAR.isSqlarTable dbPath (T.pack tableName)
      if isSqlar
        then doMkColSqlar dbPath tableName rest
        else do
          traceM $ show urlPath ++ " collection created"
          pure NoContent
    _ -> do
      traceM $ show urlPath ++ " collection created"
      pure NoContent


doMkColSqlar :: FilePath -> String -> [String] -> Handler NoContent
doMkColSqlar dbPath tableName rest = do
  let archive = SQLAR.archivePath rest
  -- RFC 4918 §9.3.1: the parent must exist (otherwise 409).
  parentOk <- liftIO $ SQLAR.parentExists dbPath (T.pack tableName) archive
  unless parentOk $
    throwError err409{errBody = "Parent collection does not exist"}
  -- And the path itself must NOT already exist (otherwise 405).
  existing <- liftIO $ SQLAR.resolvePath dbPath (T.pack tableName) archive
  when (isJust existing) $
    throwError err405{errBody = "Resource already exists"}
  now <- liftIO getPOSIXTime
  let
    -- 0o040755 == directory with 0755 perms
    mode :: Integer
    mode = 0o040755
  liftIO $
    SQLAR.insertEntry
      dbPath
      (T.pack tableName)
      archive
      (fromInteger mode)
      (truncate now)
      ByteString.empty
  pure NoContent


propNameToEntry ::
  FilePath ->
  [String] ->
  ItemType ->
  String ->
  Handler (Maybe (String, String))
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
              Query $
                "SELECT * FROM "
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


getPropsForTable ::
  FilePath ->
  [String] ->
  Maybe Text ->
  [String] ->
  String ->
  Handler [PropResults]
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

  pure $
    rootPropResult
      : if depthLow /= Just "1" && depthLow /= Just "infinity"
        then []
        else ioTableRows


getPropsForRow ::
  FilePath ->
  [String] ->
  Maybe Text ->
  [String] ->
  String ->
  Maybe Integer ->
  Handler [PropResults]
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

  when (null rowColumns) $
    throwError
      err404{errBody = "Row does not exist or does not have any columns"}

  propResults <-
    mapM
      ( \rowColumn -> do
          let
            colName = T.unpack (fst rowColumn)
          fileExt <- liftIO $ dataToFileExt (snd rowColumn)
          let
            getPropName tableName =
              tableName
                ++ "/"
                ++ show rowid
                ++ "/"
                ++ colName
                ++ fileExt

          props <-
            propNames
              & mapM (propNameToEntry dbPath (urlPath ++ [colName]) File)
              <&> catMaybes

          pure $
            PropResults
              { propName = getPropName tableName
              , itemType = File
              , props
              , propMissing = propNames & keepMissingNames File
              }
      )
      rowColumns

  pure $
    rootPropResult
      : if depthLow /= Just "1" && depthLow /= Just "infinity"
        then []
        else propResults


getPropsForCell ::
  FilePath ->
  [String] ->
  [String] ->
  String ->
  Maybe Integer ->
  String ->
  Handler [PropResults]
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


sqlarPropEntry ::
  FilePath ->
  ItemType ->
  Integer ->
  Maybe UTCTime ->
  String ->
  Handler (Maybe (String, String))
sqlarPropEntry dbPath theItemType size mtime propNameStr =
  case propNameStr of
    "creationdate" -> pure Nothing
    "getcontentlength" ->
      pure $ case theItemType of
        File -> Just (propNameStr, show size)
        Folder -> Nothing
    "getlastmodified" -> do
      txt <- case mtime of
        Just t -> pure (formatTimestamp t)
        Nothing -> do
          dbMtime <- liftIO $ getModificationTime dbPath
          pure (formatTimestamp dbMtime)
      pure $ Just (propNameStr, T.unpack txt)
    _ -> pure Nothing


sqlarEntryToPropResults ::
  FilePath ->
  [String] ->
  String ->
  SQLAR.SqlarEntry ->
  Handler PropResults
sqlarEntryToPropResults dbPath propNames pName entry = do
  let entryItemType = SQLAR.entryType entry
  pairs <-
    propNames
      & mapM
        ( sqlarPropEntry
            dbPath
            entryItemType
            (SQLAR.entrySize entry)
            (SQLAR.entryMtime entry)
        )
      <&> catMaybes
  pure $
    PropResults
      { propName = pName
      , itemType = entryItemType
      , props = pairs
      , propMissing = propNames & keepMissingNames entryItemType
      }


getPropsForSqlar ::
  FilePath ->
  Maybe Text ->
  [String] ->
  String ->
  [String] ->
  Handler [PropResults]
getPropsForSqlar dbPath depth propNames tableName restPath = do
  let
    restPathClean = restPath & filter (/= "")
    archive = SQLAR.archivePath restPathClean

  case restPathClean of
    [] -> pure ()
    _ -> ignoreHiddenFiles (fromMaybe "" (lastMay restPathClean))

  resolved <- liftIO $ SQLAR.resolvePath dbPath (T.pack tableName) archive
  case resolved of
    Nothing ->
      throwError err404{errBody = "Path not found in archive"}
    Just entry -> do
      let
        rootHref =
          if T.null archive
            then tableName
            else tableName ++ "/" ++ T.unpack archive
        depthLow = depth <&> toLower
        isDeep = depthLow == Just "1" || depthLow == Just "infinity"
        -- Use the archive-root href when restPath is empty,
        -- which gives the table a folder identity.
        rootEntry = case restPathClean of
          [] -> SQLAR.rootEntry
          _ -> entry

      rootResult <- sqlarEntryToPropResults dbPath propNames rootHref rootEntry

      case SQLAR.entryType entry of
        File -> pure [rootResult]
        Folder
          | not isDeep -> pure [rootResult]
          | otherwise -> do
              children <-
                liftIO $ SQLAR.listAt dbPath (T.pack tableName) archive
              childResults <-
                children
                  & mapM
                    ( \child -> do
                        let childHref =
                              tableName
                                ++ "/"
                                ++ T.unpack (SQLAR.entryFullName child)
                        sqlarEntryToPropResults dbPath propNames childHref child
                    )
              pure (rootResult : childResults)


-- | Subset of RFC 8144 / 7240 Prefer header values that the server honors.
data Preferences = Preferences
  { depthNoroot :: Bool
  , returnMinimal :: Bool
  }


emptyPreferences :: Preferences
emptyPreferences = Preferences{depthNoroot = False, returnMinimal = False}


parsePrefer :: Maybe Text -> Preferences
parsePrefer Nothing = emptyPreferences
parsePrefer (Just header) =
  let
    tokens =
      header
        & toLower
        & T.splitOn ","
        & fmap (T.takeWhile (/= ';'))
        & fmap (T.filter (not . isSpace))
  in
    Preferences
      { depthNoroot = "depth-noroot" `elem` tokens
      , returnMinimal = "return=minimal" `elem` tokens
      }


{-| Apply the requested preferences and return what was actually applied.
`depth-noroot` only applies when Depth is "1" or "infinity"
(RFC 8144, Section 2.1).
-}
applyPreferences ::
  Preferences ->
  Maybe Text ->
  [PropResults] ->
  (Preferences, [PropResults])
applyPreferences prefs depth results =
  let
    depthLow = depth <&> toLower
    isDepthHigh = depthLow == Just "1" || depthLow == Just "infinity"
    dropRoot = prefs.depthNoroot && isDepthHigh
    afterRoot = case (dropRoot, results) of
      (True, _ : rest) -> rest
      _ -> results
    afterMinimal =
      if prefs.returnMinimal
        then afterRoot <&> \r -> r{propMissing = []}
        else afterRoot
    appliedPrefs =
      Preferences
        { depthNoroot = dropRoot
        , returnMinimal = prefs.returnMinimal
        }
  in
    (appliedPrefs, afterMinimal)


preferenceAppliedHeader :: Preferences -> Maybe String
preferenceAppliedHeader prefs =
  let
    parts =
      ["depth-noroot" | prefs.depthNoroot]
        ++ ["return=minimal" | prefs.returnMinimal]
  in
    case parts of
      [] -> Nothing
      xs -> Just (intercalate ", " xs)


doPropFind ::
  String ->
  [String] ->
  Maybe Text ->
  Maybe Text ->
  Element ->
  Handler
    (Headers '[Header "Preference-Applied" String] [PropResults])
doPropFind dbPath urlPath depth preferMb doc = do
  let
    urlPathNorm = urlPath & filter (/= "")
    propNames =
      [ qName $ elName x
      | Elem x <- concatMap elContent ([x | Elem x <- elContent doc])
      ]
    prefs = parsePrefer preferMb

  results <- case urlPathNorm of
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

      pure $
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
                    , props = folderProps
                    , propMissing = propNames & keepMissingNames Folder
                    }
    --
    tableName : restPath -> do
      isSqlar <- liftIO $ SQLAR.isSqlarTable dbPath (T.pack tableName)
      if isSqlar
        then getPropsForSqlar dbPath depth propNames tableName restPath
        else case restPath of
          [] ->
            getPropsForTable dbPath urlPathNorm depth propNames tableName
          [rowName] ->
            getPropsForRow
              dbPath
              urlPathNorm
              depth
              propNames
              tableName
              (readMaybe rowName)
          rowName : colNameWithExt : _rest ->
            getPropsForCell
              dbPath
              urlPathNorm
              propNames
              tableName
              (readMaybe rowName)
              colNameWithExt

  let (appliedPrefs, filtered) = applyPreferences prefs depth results
  pure $ case preferenceAppliedHeader appliedPrefs of
    Nothing -> noHeader filtered
    Just hdr -> addHeader hdr filtered


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
