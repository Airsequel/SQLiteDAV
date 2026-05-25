{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| Support for SQLite Archive Files (sqlar).

A table is treated as a sqlar archive when its schema matches
the standard sqlar specification:

@
CREATE TABLE sqlar(
  name TEXT PRIMARY KEY,
  mode INT,
  mtime INT,
  sz   INT,
  data BLOB
);
@

See <https://www.sqlite.org/sqlar.html>.
-}
module SQLiteDAV.SQLAR (
  SqlarEntry (..),
  SqlarFile (..),
  isSqlarTable,
  listAt,
  lookupEntry,
  hasPath,
  resolvePath,
  decompressData,
  insertEntry,
  deleteEntry,
  deleteSubtree,
  archivePath,
  rootEntry,
) where

import Protolude (
  Bool (..),
  Char,
  Eq,
  FilePath,
  IO,
  Int,
  Integer,
  Maybe (..),
  Show,
  Text,
  fmap,
  fromIntegral,
  not,
  otherwise,
  pure,
  ($),
  (&&),
  (.),
  (<),
  (<=),
  (<>),
  (==),
  (>),
  (||),
 )

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.SQLite.Simple (
  Only (..),
  Query (Query),
  SQLData (..),
  execute,
  query,
  query_,
  withConnection,
 )

import Codec.Compression.Zlib qualified as Zlib

import SQLiteDAV.Properties (ItemType (File, Folder))


-- | A listing entry directly under some archive path.
data SqlarEntry = SqlarEntry
  { entryName :: Text
  -- ^ Path relative to the listing prefix (e.g. "foo.txt" or "subdir").
  , entryFullName :: Text
  -- ^ Full path inside the archive (without leading slash).
  , entryType :: ItemType
  , entrySize :: Integer
  -- ^ Reported file size (column @sz@). Zero for folders.
  , entryMtime :: Maybe UTCTime
  }
  deriving (Show, Eq)


-- | A resolved file entry from the archive.
data SqlarFile = SqlarFile
  { fileContent :: ByteString
  , fileMtime :: Maybe UTCTime
  , fileSize :: Integer
  }
  deriving (Show)


sqlarColumns :: [Text]
sqlarColumns = ["name", "mode", "mtime", "sz", "data"]


{-| True if @tableName@ has the sqlar schema (the columns
@name@, @mode@, @mtime@, @sz@, @data@).
-}
isSqlarTable :: FilePath -> Text -> IO Bool
isSqlarTable dbPath tableName =
  withConnection dbPath $ \conn -> do
    cols :: [Only Text] <-
      query
        conn
        "SELECT name FROM pragma_table_info(?)"
        (Only tableName)
    let names = fmap (\(Only n) -> n) cols
    pure $ List.all (`List.elem` names) sqlarColumns


{-| Normalize the listing prefix so it always ends with a slash
(unless empty, which means "root of archive").
-}
normalizePrefix :: Text -> Text
normalizePrefix p
  | T.null p = ""
  | T.isSuffixOf "/" p = p
  | otherwise = p <> "/"


-- | Build the archive-internal path from URL path segments.
archivePath :: [[Char]] -> Text
archivePath segments =
  segments
    & List.filter (not . List.null)
    & fmap T.pack
    & T.intercalate "/"


{-| List entries directly under @prefix@ (which must end with @/@
or be empty for the archive root).
-}
listAt :: FilePath -> Text -> Text -> IO [SqlarEntry]
listAt dbPath tableName rawPrefix = do
  let prefix = normalizePrefix rawPrefix
  withConnection dbPath $ \conn -> do
    let q =
          Query $
            "SELECT name, mode, mtime, sz, data FROM "
              <> quoteIdent tableName
    rows :: [(Text, SQLData, SQLData, SQLData, SQLData)] <- query_ conn q
    pure $ dedupe (catMaybes (fmap (rowToEntry prefix) rows))


rowToEntry ::
  Text ->
  (Text, SQLData, SQLData, SQLData, SQLData) ->
  Maybe SqlarEntry
rowToEntry prefix (name, modeData, mtimeData, szData, dataCol) =
  let
    canonName =
      if T.isSuffixOf "/" name && T.length name > 1
        then T.dropEnd 1 name
        else name

    inScope =
      T.null prefix
        || T.isPrefixOf prefix canonName
        || T.isPrefixOf prefix (canonName <> "/")

    relative =
      if T.null prefix
        then canonName
        else T.drop (T.length prefix) canonName

    isSelf = T.null relative

    (segment, rest) = T.break (== '/') relative

    isExplicitFolder =
      T.isSuffixOf "/" name
        || isDirMode modeData
        || (sqlIsNull dataCol && szIsZero szData)

    childType =
      if T.null rest && not isExplicitFolder
        then File
        else Folder

    fullName =
      if T.null prefix
        then segment
        else prefix <> segment

    sz = case childType of
      File -> sqlInt szData
      Folder -> 0
  in
    if not inScope || isSelf || T.null segment
      then Nothing
      else
        Just
          SqlarEntry
            { entryName = segment
            , entryFullName = fullName
            , entryType = childType
            , entrySize = sz
            , entryMtime = sqlMtime mtimeData
            }


{-| Collapse duplicates produced by multiple rows that share the
same first segment. Folder type wins when both appear.
-}
dedupe :: [SqlarEntry] -> [SqlarEntry]
dedupe entries =
  let
    sorted = List.sortOn entryName entries
    byName = List.groupBy (\a b -> a.entryName == b.entryName) sorted
  in
    fmap mergeGroup byName
  where
    -- `groupBy` always produces non-empty groups, so the fallback
    -- pattern match below is exhaustive.
    mergeGroup grp =
      let
        folder = List.find (\e -> e.entryType == Folder) grp
      in
        case folder of
          Just f -> f
          Nothing -> case grp of
            (x : _) -> x
            [] -> rootEntry


-- | Look up a single file entry by exact archive path.
lookupEntry :: FilePath -> Text -> Text -> IO (Maybe SqlarFile)
lookupEntry dbPath tableName path =
  withConnection dbPath $ \conn -> do
    let canon =
          if T.isSuffixOf "/" path && T.length path > 1
            then T.dropEnd 1 path
            else path
        q =
          Query $
            "SELECT sz, mtime, data FROM "
              <> quoteIdent tableName
              <> " WHERE name = ? OR name = ?"
    rows :: [(SQLData, SQLData, SQLData)] <-
      query conn q (canon, canon <> "/")
    pure $ case rows of
      [] -> Nothing
      ((szData, mtimeData, dataCol) : _) ->
        let
          sz = sqlInt szData
        in
          Just
            SqlarFile
              { fileContent = decompressData dataCol sz
              , fileMtime = sqlMtime mtimeData
              , fileSize = sz
              }


-- | Virtual entry representing the archive root.
rootEntry :: SqlarEntry
rootEntry =
  SqlarEntry
    { entryName = ""
    , entryFullName = ""
    , entryType = Folder
    , entrySize = 0
    , entryMtime = Nothing
    }


{-| Resolve a path inside the archive to its listing entry.
Returns 'Nothing' if the path neither exists as a stored row nor
appears as an implicit folder prefix.
-}
resolvePath :: FilePath -> Text -> Text -> IO (Maybe SqlarEntry)
resolvePath dbPath tableName path
  | T.null path = pure (Just rootEntry)
  | otherwise = do
      let canon =
            if T.isSuffixOf "/" path && T.length path > 1
              then T.dropEnd 1 path
              else path
          (parent, name) = T.breakOnEnd "/" canon
      entries <- listAt dbPath tableName parent
      pure $ List.find (\e -> e.entryName == name) entries


{-| True if a path exists in the archive, either as an explicit
entry or as an implicit folder containing other entries.
-}
hasPath :: FilePath -> Text -> Text -> IO Bool
hasPath dbPath tableName path
  | T.null path = pure True
  | otherwise = withConnection dbPath $ \conn -> do
      let canon =
            if T.isSuffixOf "/" path && T.length path > 1
              then T.dropEnd 1 path
              else path
          q =
            Query $
              "SELECT 1 FROM "
                <> quoteIdent tableName
                <> " WHERE name = ? OR name = ? OR name LIKE ? LIMIT 1"
      rows :: [Only Int] <-
        query conn q (canon, canon <> "/", canon <> "/%")
      pure $ not (List.null rows)


{-| Decompress a data cell against its declared size.
If @sz@ equals the blob length, the data is stored uncompressed.
-}
decompressData :: SQLData -> Integer -> ByteString
decompressData sqlData declaredSize =
  case sqlData of
    SQLBlob bs
      | fromIntegral (BS.length bs) == declaredSize -> bs
      | declaredSize <= 0 -> bs
      | otherwise ->
          BL.toStrict (Zlib.decompress (BL.fromStrict bs))
    SQLText t -> T.encodeUtf8 t
    _ -> BS.empty


-- | Insert or replace an archive entry (stored uncompressed).
insertEntry ::
  FilePath ->
  Text ->
  Text ->
  Int ->
  Integer ->
  ByteString ->
  IO ()
insertEntry dbPath tableName name mode mtime payload =
  withConnection dbPath $ \conn -> do
    let sz = fromIntegral (BS.length payload) :: Integer
        q =
          Query $
            "INSERT OR REPLACE INTO "
              <> quoteIdent tableName
              <> " (name, mode, mtime, sz, data) VALUES (?, ?, ?, ?, ?)"
    execute conn q (name, mode, mtime, sz, SQLBlob payload)


-- | Delete a single archive entry by exact name.
deleteEntry :: FilePath -> Text -> Text -> IO ()
deleteEntry dbPath tableName name =
  withConnection dbPath $ \conn -> do
    let canon =
          if T.isSuffixOf "/" name && T.length name > 1
            then T.dropEnd 1 name
            else name
        q =
          Query $
            "DELETE FROM "
              <> quoteIdent tableName
              <> " WHERE name = ? OR name = ?"
    execute conn q (canon, canon <> "/")


-- | Delete everything under a folder path (recursive).
deleteSubtree :: FilePath -> Text -> Text -> IO ()
deleteSubtree dbPath tableName prefix =
  withConnection dbPath $ \conn -> do
    let canon =
          if T.isSuffixOf "/" prefix && T.length prefix > 1
            then T.dropEnd 1 prefix
            else prefix
        q =
          Query $
            "DELETE FROM "
              <> quoteIdent tableName
              <> " WHERE name = ? OR name = ? OR name LIKE ?"
    execute conn q (canon, canon <> "/", canon <> "/%")


-- Helpers --------------------------------------------------------------------

sqlIsNull :: SQLData -> Bool
sqlIsNull SQLNull = True
sqlIsNull _ = False


szIsZero :: SQLData -> Bool
szIsZero (SQLInteger 0) = True
szIsZero SQLNull = True
szIsZero _ = False


sqlInt :: SQLData -> Integer
sqlInt (SQLInteger i) = fromIntegral i
sqlInt _ = 0


sqlMtime :: SQLData -> Maybe UTCTime
sqlMtime (SQLInteger i) = Just (posixSecondsToUTCTime (fromIntegral i))
sqlMtime _ = Nothing


-- | True when @mode@ encodes a Unix directory (S_IFMT bits = S_IFDIR).
isDirMode :: SQLData -> Bool
isDirMode (SQLInteger i) = (fromIntegral i .&. 0o170000) == (0o040000 :: Int)
isDirMode _ = False


escDoubleQuotes :: Text -> Text
escDoubleQuotes = T.replace "\"" "\"\""


quoteIdent :: Text -> Text
quoteIdent kw = "\"" <> escDoubleQuotes kw <> "\""
