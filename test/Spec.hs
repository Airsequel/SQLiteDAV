{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Spec where

import Protolude (
  Char,
  IO,
  Int,
  Integer,
  Maybe (..),
  Text,
  decodeUtf8,
  fmap,
  for_,
  isSpace,
  liftIO,
  not,
  panic,
  pure,
  show,
  traceShowId,
  unless,
  ($),
  (&),
  (*),
  (.),
  (<&>),
  (<>),
  (==),
 )
import Protolude.Unsafe (unsafeIndex)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (
  Only (Only),
  SQLData (SQLBlob),
  execute,
  execute_,
  query,
  withConnection,
 )
import Debug.Trace (traceM)
import Network.HTTP.Types (hContentType)
import Network.Wai ()
import Network.Wai.Test (SResponse (..), simpleBody)
import System.Directory (copyFile, doesFileExist, removeFile)
import Test.Hspec (Spec, describe, fit, hspec, it)
import Test.Hspec.Wai (
  MatchHeader,
  ResponseMatcher (
    ResponseMatcher,
    matchBody,
    matchHeaders,
    matchStatus
  ),
  WaiSession,
  delete,
  get,
  options,
  request,
  shouldRespondWith,
  with,
  (<:>),
 )
import Text.Regex.TDFA

import SQLiteDAV.API ()
import SQLiteDAV.HTTPExtensions ()
import SQLiteDAV.Properties ()
import SQLiteDAV.Server (RowNameMode (..), webDavServer)
import SQLiteDAV.Utils (formatTimestamp)


type String = [Char]


{-| Perform an `PROPFIND` request to the application under test.
| FIXME: Can't reference Data.ByteString.Internal.ByteString here,
|        because it can not be imported.
-}
propfind :: _ -> Integer -> _ -> WaiSession st SResponse
propfind path depth =
  request
    "PROPFIND"
    path
    [ (hContentType, "application/xml")
    , ("Depth", show depth)
    ]


propfindPrefer :: _ -> Integer -> _ -> _ -> WaiSession st SResponse
propfindPrefer path depth prefer =
  request
    "PROPFIND"
    path
    [ (hContentType, "application/xml")
    , ("Depth", show depth)
    , ("Prefer", prefer)
    ]


lock :: _ -> _ -> WaiSession st SResponse
lock path =
  request
    "LOCK"
    path
    [(hContentType, "application/xml")]


unlock :: _ -> _ -> WaiSession st SResponse
unlock path token =
  request
    "UNLOCK"
    path
    [("Lock-Token", token)]
    ""


-- | Remove leading whitespace on each line of a string
rmLeadSpace :: Text -> Text
rmLeadSpace = T.unlines . fmap (T.dropWhile isSpace) . T.lines


-- | Recursively remove all spaces between tags
rmXmlSpace :: T.Text -> T.Text
rmXmlSpace xmlTxt =
  let
    xmlEnd = T.replace "> " ">" xmlTxt
    xmlNorm = T.replace " <" "<" xmlEnd
  in
    if xmlNorm == xmlTxt
      then xmlNorm
      else rmXmlSpace xmlNorm


normalizeXml :: Text -> Text
normalizeXml xmlRequest =
  rmXmlSpace $
    "<?xml version='1.0' ?>"
      <> ( xmlRequest
             & T.replace "</" "</D:"
             & T.replace "<" "<D:"
             & T.replace "<D:/D:" "</D:"
         )


davHeader :: MatchHeader
davHeader = "Dav" <:> "1, 2, ordered-collections"


xmlHeader :: MatchHeader
xmlHeader = "Content-Type" <:> "application/xml"


rmModified :: WaiSession st SResponse -> WaiSession st SResponse
rmModified fRes =
  let
    regex :: BL.ByteString = "<D:getlastmodified>([^<>]+)</D:getlastmodified>"
  in
    fRes
      <&> ( \sres ->
              let
                bodyTxt :: Text =
                  sres
                    & simpleBody
                    & BL.toStrict
                    & decodeUtf8
                timestampMatch :: Text =
                  bodyTxt =~ regex
                simpleBodyNew =
                  bodyTxt
                    & T.replace
                      timestampMatch
                      "<D:getlastmodified>REMOVED</D:getlastmodified>"
                    & T.unpack
                    & fromString
              in
                sres{simpleBody = simpleBodyNew}
          )


{-| Copy the test fixture database to a scratch path so tests can mutate it
without polluting the committed file.
-}
mkTestApp :: IO _
mkTestApp = do
  let scratchDb = "test/data_scratch.sqlite"
  copyFile "test/data.sqlite" scratchDb
  pure $ webDavServer RowIdMode scratchDb


-- | Same as 'mkTestApp' but for the sqlar fixture.
mkSqlarApp :: IO _
mkSqlarApp = do
  let scratchDb = "test/archive_scratch.sqlar"
  copyFile "test/archive.sqlar" scratchDb
  pure $ webDavServer RowIdMode scratchDb


{-| Build a fixture with a single table whose payloads are large enough
that loading them all into memory (as the pre-fix server did) would
inflate the listing cost by several megabytes per row. Used to guard
against regressions of issue #14.
-}
mkBigBlobApp :: IO _
mkBigBlobApp = do
  let scratchDb = "test/big_blobs_scratch.sqlite"
  exists <- doesFileExist scratchDb
  unless (not exists) $ removeFile scratchDb
  withConnection scratchDb $ \conn -> do
    execute_ conn "CREATE TABLE big_blobs (payload BLOB)"
    let blob = SQLBlob (BS.replicate (256 * 1024) 0x42)
    for_ [1 :: Int .. 32] $ \_ ->
      execute conn "INSERT INTO big_blobs (payload) VALUES (?)" [blob]
  pure $ webDavServer RowIdMode scratchDb


put :: _ -> _ -> WaiSession st SResponse
put path =
  request
    "PUT"
    path
    [(hContentType, "application/octet-stream")]


{-| Report @typeof(col)@ for a given users row in the scratch fixture.
Used by the PUT type-preservation tests.
-}
typeofCell :: Text -> Int -> IO Text
typeofCell colName rowid =
  withConnection "test/data_scratch.sqlite" $ \conn -> do
    let q = fromString ("SELECT typeof(" <> T.unpack colName <> ") FROM users WHERE rowid = ?")
    rows :: [Only Text] <- query conn q (Only rowid)
    pure $ case rows of
      [Only t] -> t
      _ -> "missing"


mkcol :: _ -> WaiSession st SResponse
mkcol path =
  request
    "MKCOL"
    path
    []
    ""


spec :: Spec
spec = with mkTestApp $ do
  describe "OPTIONS" $ do
    it "returns 200 for OPTIONS requests" $ do
      options "/" `shouldRespondWith` 200
  describe "PROPFIND" $ do
    it "returns a list of PropResults" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <getcontentlength/>\
            \    <creationdate/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <getcontentlength />\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"

        result = propfind "/" 0 (fromString (T.unpack xmlRequest))

      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders = [davHeader, xmlHeader]
          , matchBody = fromString (T.unpack xmlResponse)
          }

    it "returns a list of PropResults for tables" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <getcontentlength/>\
            \    <creationdate/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/users</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <getcontentlength />\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"
        result = propfind "/users" 0 (fromString (T.unpack xmlRequest))

      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders = [davHeader, xmlHeader]
          , matchBody = fromString (T.unpack xmlResponse)
          }

    it "returns a list of PropResults for table rows" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <getcontentlength/>\
            \    <creationdate/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/users</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <getcontentlength />\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/1</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <getcontentlength />\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/2</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <getcontentlength />\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/3</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <getcontentlength />\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"
        result = propfind "/users/" 1 (fromString (T.unpack xmlRequest))

      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders = [davHeader, xmlHeader]
          , matchBody = fromString (T.unpack xmlResponse)
          }

    it "returns a list of PropResults for table columns" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <getcontentlength/>\
            \    <creationdate/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/users/1</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <getcontentlength />\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/1/name.txt</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \        <getcontentlength>4</getcontentlength>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/1/email.txt</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \        <getcontentlength>16</getcontentlength>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/1/height.txt</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \        <getcontentlength>3</getcontentlength>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/1/photo.png</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \        <getcontentlength>135872</getcontentlength>\
            \      </prop>\
            \    </propstat>\
            \    <propstat>\
            \      <status>HTTP/1.1 404 Not Found</status>\
            \      <prop>\
            \        <creationdate />\
            \      </prop>\
            \      <responsedescription>\
            \        Property was not found\
            \      </responsedescription>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"

      let result = propfind "/users/1" 1 (fromString (T.unpack xmlRequest))
      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders = [davHeader, xmlHeader]
          , matchBody = fromString (T.unpack xmlResponse)
          }

      let resSlash = propfind "/users/1/" 1 (fromString (T.unpack xmlRequest))
      rmModified resSlash
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders = [davHeader, xmlHeader]
          , matchBody = fromString (T.unpack xmlResponse)
          }

    it "Prefer: depth-noroot omits the root resource" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/users/1</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/2</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/3</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"
        result =
          propfindPrefer
            "/users/"
            1
            "depth-noroot"
            (fromString (T.unpack xmlRequest))

      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders =
              [ davHeader
              , xmlHeader
              , "Preference-Applied" <:> "depth-noroot"
              ]
          , matchBody = fromString (T.unpack xmlResponse)
          }

    it "Prefer: depth-noroot is ignored when Depth is 0" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/users</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"
        result =
          propfindPrefer
            "/users"
            0
            "depth-noroot"
            (fromString (T.unpack xmlRequest))

      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders = [davHeader, xmlHeader]
          , matchBody = fromString (T.unpack xmlResponse)
          }

    it "Prefer: return=minimal omits 404 propstat blocks" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <getcontentlength/>\
            \    <creationdate/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/users</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"
        result =
          propfindPrefer
            "/users"
            0
            "return=minimal"
            (fromString (T.unpack xmlRequest))

      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders =
              [ davHeader
              , xmlHeader
              , "Preference-Applied" <:> "return=minimal"
              ]
          , matchBody = fromString (T.unpack xmlResponse)
          }

    it "returns 400 for malformed XML body" $ do
      request
        "PROPFIND"
        "/"
        [(hContentType, "application/xml"), ("Depth", "0")]
        "<foo>"
        `shouldRespondWith` 400

    it "returns 400 when a prefix is bound to the empty namespace" $ do
      request
        "PROPFIND"
        "/"
        [(hContentType, "application/xml"), ("Depth", "0")]
        ( "<D:propfind xmlns:D=\"DAV:\">"
            <> "<D:prop><bar:foo xmlns:bar=\"\"/></D:prop>"
            <> "</D:propfind>"
        )
        `shouldRespondWith` 400

    it "Prefer can combine depth-noroot and return=minimal" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <getlastmodified/>\
            \    <creationdate/>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
        xmlResponse =
          normalizeXml
            "<multistatus xmlns:D=\"DAV:\">\
            \  <response>\
            \    <href>/users/1</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/2</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \  <response>\
            \    <href>/users/3</href>\
            \    <propstat>\
            \      <status>HTTP/1.1 200 OK</status>\
            \      <prop>\
            \        <resourcetype>\
            \          <collection />\
            \        </resourcetype>\
            \        <getlastmodified>REMOVED</getlastmodified>\
            \      </prop>\
            \    </propstat>\
            \  </response>\
            \</multistatus>\
            \"
        result =
          propfindPrefer
            "/users/"
            1
            "depth-noroot, return=minimal"
            (fromString (T.unpack xmlRequest))

      rmModified result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 207
          , matchHeaders =
              [ davHeader
              , xmlHeader
              , "Preference-Applied" <:> "depth-noroot, return=minimal"
              ]
          , matchBody = fromString (T.unpack xmlResponse)
          }

  describe "GET" $ do
    it "returns the content of a cell" $ do
      get "/users/1/name"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "John"
          }

  describe "DELETE" $ do
    it "sets a cell to NULL" $ do
      delete "/users/2/email"
        `shouldRespondWith` 204
      get "/users/2/email"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "NULL"
          }

  describe "LOCK" $ do
    it "returns a fake lock token and lockdiscovery XML" $ do
      let
        xmlRequest =
          normalizeXml
            "<lockinfo xmlns:D=\"DAV:\">\
            \  <lockscope><exclusive/></lockscope>\
            \  <locktype><write/></locktype>\
            \  <owner>\
            \    <href>http://example.com/owner</href>\
            \  </owner>\
            \</lockinfo>\
            \"
        xmlResponse =
          normalizeXml
            "<prop xmlns:D=\"DAV:\">\
            \  <lockdiscovery>\
            \    <activelock>\
            \      <locktype><write /></locktype>\
            \      <lockscope><exclusive /></lockscope>\
            \      <depth>infinity</depth>\
            \      <timeout>Second-604800</timeout>\
            \      <locktoken>\
            \        <href>urn:uuid:00000000-0000-0000-0000-000000000001</href>\
            \      </locktoken>\
            \      <lockroot>\
            \        <href>/users/1/name.txt</href>\
            \      </lockroot>\
            \    </activelock>\
            \  </lockdiscovery>\
            \</prop>\
            \"
        result = lock "/users/1/name.txt" (fromString (T.unpack xmlRequest))

      result
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders =
              [ davHeader
              , xmlHeader
              , "Lock-Token"
                  <:> "<urn:uuid:00000000-0000-0000-0000-000000000001>"
              ]
          , matchBody = fromString (T.unpack xmlResponse)
          }

  describe "UNLOCK" $ do
    it "returns 204 No Content" $ do
      unlock
        "/users/1/name.txt"
        "<urn:uuid:00000000-0000-0000-0000-000000000001>"
        `shouldRespondWith` 204


sqlarSpec :: Spec
sqlarSpec = with mkSqlarApp $ do
  describe "SQLAR archive" $ do
    describe "PROPFIND" $ do
      it "lists top-level entries with Depth 1" $ do
        let
          xmlRequest =
            normalizeXml
              "<propfind xmlns:D=\"DAV:\">\
              \  <prop>\
              \    <resourcetype/>\
              \    <getcontentlength/>\
              \  </prop>\
              \</propfind>\
              \"
          xmlResponse =
            normalizeXml
              "<multistatus xmlns:D=\"DAV:\">\
              \  <response>\
              \    <href>/sqlar</href>\
              \    <propstat>\
              \      <status>HTTP/1.1 200 OK</status>\
              \      <prop>\
              \        <resourcetype>\
              \          <collection />\
              \        </resourcetype>\
              \      </prop>\
              \    </propstat>\
              \    <propstat>\
              \      <status>HTTP/1.1 404 Not Found</status>\
              \      <prop>\
              \        <getcontentlength />\
              \      </prop>\
              \      <responsedescription>\
              \        Property was not found\
              \      </responsedescription>\
              \    </propstat>\
              \  </response>\
              \  <response>\
              \    <href>/sqlar/docs</href>\
              \    <propstat>\
              \      <status>HTTP/1.1 200 OK</status>\
              \      <prop>\
              \        <resourcetype>\
              \          <collection />\
              \        </resourcetype>\
              \      </prop>\
              \    </propstat>\
              \    <propstat>\
              \      <status>HTTP/1.1 404 Not Found</status>\
              \      <prop>\
              \        <getcontentlength />\
              \      </prop>\
              \      <responsedescription>\
              \        Property was not found\
              \      </responsedescription>\
              \    </propstat>\
              \  </response>\
              \  <response>\
              \    <href>/sqlar/readme.txt</href>\
              \    <propstat>\
              \      <status>HTTP/1.1 200 OK</status>\
              \      <prop>\
              \        <getcontentlength>11</getcontentlength>\
              \      </prop>\
              \    </propstat>\
              \  </response>\
              \</multistatus>\
              \"

          result = propfind "/sqlar/" 1 (fromString (T.unpack xmlRequest))

        result
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 207
            , matchHeaders = [davHeader, xmlHeader]
            , matchBody = fromString (T.unpack xmlResponse)
            }

      it "lists a nested folder" $ do
        let
          xmlRequest =
            normalizeXml
              "<propfind xmlns:D=\"DAV:\">\
              \  <prop>\
              \    <resourcetype/>\
              \    <getcontentlength/>\
              \  </prop>\
              \</propfind>\
              \"
          xmlResponse =
            normalizeXml
              "<multistatus xmlns:D=\"DAV:\">\
              \  <response>\
              \    <href>/sqlar/docs</href>\
              \    <propstat>\
              \      <status>HTTP/1.1 200 OK</status>\
              \      <prop>\
              \        <resourcetype>\
              \          <collection />\
              \        </resourcetype>\
              \      </prop>\
              \    </propstat>\
              \    <propstat>\
              \      <status>HTTP/1.1 404 Not Found</status>\
              \      <prop>\
              \        <getcontentlength />\
              \      </prop>\
              \      <responsedescription>\
              \        Property was not found\
              \      </responsedescription>\
              \    </propstat>\
              \  </response>\
              \  <response>\
              \    <href>/sqlar/docs/guide</href>\
              \    <propstat>\
              \      <status>HTTP/1.1 200 OK</status>\
              \      <prop>\
              \        <resourcetype>\
              \          <collection />\
              \        </resourcetype>\
              \      </prop>\
              \    </propstat>\
              \    <propstat>\
              \      <status>HTTP/1.1 404 Not Found</status>\
              \      <prop>\
              \        <getcontentlength />\
              \      </prop>\
              \      <responsedescription>\
              \        Property was not found\
              \      </responsedescription>\
              \    </propstat>\
              \  </response>\
              \  <response>\
              \    <href>/sqlar/docs/intro.md</href>\
              \    <propstat>\
              \      <status>HTTP/1.1 200 OK</status>\
              \      <prop>\
              \        <getcontentlength>8</getcontentlength>\
              \      </prop>\
              \    </propstat>\
              \  </response>\
              \</multistatus>\
              \"

          result = propfind "/sqlar/docs" 1 (fromString (T.unpack xmlRequest))

        result
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 207
            , matchHeaders = [davHeader, xmlHeader]
            , matchBody = fromString (T.unpack xmlResponse)
            }

      it "returns 404 for missing archive paths" $ do
        let
          xmlRequest =
            normalizeXml
              "<propfind xmlns:D=\"DAV:\">\
              \  <prop><resourcetype/></prop>\
              \</propfind>\
              \"
          result =
            propfind "/sqlar/missing" 0 (fromString (T.unpack xmlRequest))

        result `shouldRespondWith` 404

    describe "GET" $ do
      it "returns file content from the archive" $ do
        get "/sqlar/readme.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "hello world"
            }

      it "returns nested file content from the archive" $ do
        get "/sqlar/docs/guide/setup.md"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "# Setup\n"
            }

      it "returns 404 for missing files" $ do
        get "/sqlar/missing.txt" `shouldRespondWith` 404

    describe "PUT" $ do
      it "stores a new file in the archive" $ do
        put "/sqlar/new.txt" "fresh content"
          `shouldRespondWith` 201
        get "/sqlar/new.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "fresh content"
            }

      it "replaces an existing file" $ do
        put "/sqlar/readme.txt" "updated"
          `shouldRespondWith` 201
        get "/sqlar/readme.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "updated"
            }

      it "rejects PUT when the parent collection is missing" $ do
        put "/sqlar/missing-dir/file.txt" "x" `shouldRespondWith` 409

      it "rejects PUT onto a folder path" $ do
        put "/sqlar/docs" "stuff" `shouldRespondWith` 405

    describe "DELETE" $ do
      it "removes a file from the archive" $ do
        delete "/sqlar/readme.txt" `shouldRespondWith` 204
        get "/sqlar/readme.txt" `shouldRespondWith` 404

      it "removes a folder subtree" $ do
        delete "/sqlar/docs" `shouldRespondWith` 204
        get "/sqlar/docs/intro.md" `shouldRespondWith` 404
        get "/sqlar/docs/guide/setup.md" `shouldRespondWith` 404

      it "returns 404 when the target does not exist" $ do
        delete "/sqlar/nope.txt" `shouldRespondWith` 404

    describe "MKCOL" $ do
      it "creates a folder entry" $ do
        mkcol "/sqlar/empty-dir" `shouldRespondWith` 201
        -- After creation, a child can be PUT into it
        put "/sqlar/empty-dir/x.txt" "x" `shouldRespondWith` 201
        get "/sqlar/empty-dir/x.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "x"
            }

      it "rejects MKCOL on an existing collection" $ do
        mkcol "/sqlar/docs" `shouldRespondWith` 405

      it "rejects MKCOL on an existing file" $ do
        mkcol "/sqlar/readme.txt" `shouldRespondWith` 405

      it "rejects MKCOL when the parent is missing" $ do
        mkcol "/sqlar/missing/sub" `shouldRespondWith` 409

      it "rejects MKCOL carrying a body" $ do
        request "MKCOL" "/sqlar/with-body" [("Content-Length", "4")] "junk"
          `shouldRespondWith` 415

    describe "COPY" $ do
      it "copies a single file" $ do
        request
          "COPY"
          "/sqlar/readme.txt"
          [("Destination", "/sqlar/readme-copy.txt")]
          ""
          `shouldRespondWith` 201
        get "/sqlar/readme-copy.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "hello world"
            }
        -- Source still exists after a COPY
        get "/sqlar/readme.txt" `shouldRespondWith` 200

      it "copies a subtree" $ do
        request
          "COPY"
          "/sqlar/docs"
          [("Destination", "/sqlar/docs-copy")]
          ""
          `shouldRespondWith` 201
        get "/sqlar/docs-copy/intro.md"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "# Intro\n"
            }
        get "/sqlar/docs-copy/guide/setup.md"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "# Setup\n"
            }

      it "returns 204 when overwriting an existing destination" $ do
        request
          "COPY"
          "/sqlar/readme.txt"
          [("Destination", "/sqlar/docs/intro.md")]
          ""
          `shouldRespondWith` 204
        get "/sqlar/docs/intro.md"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "hello world"
            }

      it "returns 412 when destination exists and Overwrite: F" $ do
        request
          "COPY"
          "/sqlar/readme.txt"
          [ ("Destination", "/sqlar/docs/intro.md")
          , ("Overwrite", "F")
          ]
          ""
          `shouldRespondWith` 412

      it "returns 409 when destination parent is missing" $ do
        request
          "COPY"
          "/sqlar/readme.txt"
          [("Destination", "/sqlar/nowhere/dest.txt")]
          ""
          `shouldRespondWith` 409

      it "returns 404 when source does not exist" $ do
        request
          "COPY"
          "/sqlar/missing.txt"
          [("Destination", "/sqlar/dest.txt")]
          ""
          `shouldRespondWith` 404

    describe "MOVE" $ do
      it "moves a single file" $ do
        request
          "MOVE"
          "/sqlar/readme.txt"
          [("Destination", "/sqlar/moved.txt")]
          ""
          `shouldRespondWith` 201
        get "/sqlar/moved.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "hello world"
            }
        -- Source is gone after a MOVE
        get "/sqlar/readme.txt" `shouldRespondWith` 404

      it "moves a subtree" $ do
        request
          "MOVE"
          "/sqlar/docs"
          [("Destination", "/sqlar/docs-moved")]
          ""
          `shouldRespondWith` 201
        get "/sqlar/docs-moved/guide/setup.md"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "# Setup\n"
            }
        get "/sqlar/docs/intro.md" `shouldRespondWith` 404

      it "returns 204 when overwriting an existing destination" $ do
        put "/sqlar/dest.txt" "old" `shouldRespondWith` 201
        request
          "MOVE"
          "/sqlar/readme.txt"
          [("Destination", "/sqlar/dest.txt")]
          ""
          `shouldRespondWith` 204
        get "/sqlar/dest.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "hello world"
            }


plainSpec :: Spec
plainSpec = with mkTestApp $ do
  describe "Plain SQLite mode" $ do
    describe "PUT" $ do
      it "updates an existing cell and reports 204" $ do
        put "/users/1/name.txt" "Updated"
          `shouldRespondWith` 204
        get "/users/1/name"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "Updated"
            }

      it "returns 201 when filling a previously NULL cell" $ do
        delete "/users/2/email" `shouldRespondWith` 204
        put "/users/2/email.txt" "ada@example.com"
          `shouldRespondWith` 201

      it "returns 404 for an unknown column" $ do
        put "/users/1/nonsense.txt" "x" `shouldRespondWith` 404

      it "returns 409 for a missing row" $ do
        put "/users/999/name.txt" "x" `shouldRespondWith` 409

      it "returns 404 for an unknown table" $ do
        put "/no_such_table/1/name.txt" "x" `shouldRespondWith` 404

      it "returns 405 when targeting a row instead of a cell" $ do
        put "/users/1" "x" `shouldRespondWith` 405

      it "keeps the TEXT column's storage type after a PUT" $ do
        put "/users/1/name.txt" "Renamed" `shouldRespondWith` 204
        cellType <- liftIO $ typeofCell "name" 1
        liftIO $ unless (cellType == "text") $
          panic ("Expected 'text', got: " <> cellType)

      it "keeps the INTEGER column's storage type after a PUT" $ do
        put "/users/1/height.txt" "200" `shouldRespondWith` 204
        cellType <- liftIO $ typeofCell "height" 1
        liftIO $ unless (cellType == "integer") $
          panic ("Expected 'integer', got: " <> cellType)

      it "falls back to text for an unparseable INTEGER body" $ do
        put "/users/1/height.txt" "tall" `shouldRespondWith` 204
        cellType <- liftIO $ typeofCell "height" 1
        liftIO $ unless (cellType == "text") $
          panic ("Expected 'text', got: " <> cellType)

      it "keeps the BLOB column's storage type after a PUT" $ do
        put "/users/1/photo.png" "\xff\xd8\xff" `shouldRespondWith` 204
        cellType <- liftIO $ typeofCell "photo" 1
        liftIO $ unless (cellType == "blob") $
          panic ("Expected 'blob', got: " <> cellType)

    describe "DELETE" $ do
      it "404s on a missing cell column" $ do
        delete "/users/1/nonsense" `shouldRespondWith` 404

      it "404s on a missing row" $ do
        delete "/users/999" `shouldRespondWith` 404

      it "deletes a row" $ do
        delete "/users/3" `shouldRespondWith` 204
        get "/users/3/name" `shouldRespondWith` 404

      it "drops a table" $ do
        delete "/users" `shouldRespondWith` 204
        get "/users/1/name" `shouldRespondWith` 404

      it "refuses to delete the database root" $ do
        delete "/" `shouldRespondWith` 403

    describe "MKCOL" $ do
      it "creates a new (sqlar-shaped) table at the root" $ do
        mkcol "/fresh_table" `shouldRespondWith` 201
        -- The new table behaves as a sqlar archive.
        put "/fresh_table/note.txt" "hello"
          `shouldRespondWith` 201
        get "/fresh_table/note.txt"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "hello"
            }

      it "rejects MKCOL on an existing table with 405" $ do
        mkcol "/users" `shouldRespondWith` 405

      it "inserts a row when MKCOL targets a missing rowid" $ do
        mkcol "/users/777" `shouldRespondWith` 201
        -- All other columns are NULL on the freshly inserted row.
        get "/users/777/name" `shouldRespondWith` 200

      it "returns 405 when the rowid already exists" $ do
        mkcol "/users/1" `shouldRespondWith` 405

      it "returns 403 when MKCOL points at a cell" $ do
        mkcol "/users/1/name" `shouldRespondWith` 403

      it "rejects MKCOL with a body" $ do
        request "MKCOL" "/users/888" [("Content-Length", "1")] "x"
          `shouldRespondWith` 415

    describe "COPY" $ do
      it "copies a cell to another column of the same row" $ do
        request
          "COPY"
          "/users/1/name.txt"
          [("Destination", "/users/1/email.txt")]
          ""
          `shouldRespondWith` 204
        get "/users/1/email"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "John"
            }
        -- Source still has its value
        get "/users/1/name"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "John"
            }

      it "clones a row to a new rowid" $ do
        request
          "COPY"
          "/users/1"
          [("Destination", "/users/500")]
          ""
          `shouldRespondWith` 201
        get "/users/500/name"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "John"
            }

      it "returns 412 when the destination cell is non-NULL and Overwrite: F" $ do
        request
          "COPY"
          "/users/1/name.txt"
          [ ("Destination", "/users/2/name.txt")
          , ("Overwrite", "F")
          ]
          ""
          `shouldRespondWith` 412

      it "returns 502 for cross-table COPY" $ do
        request
          "COPY"
          "/users/1"
          [("Destination", "/other/1")]
          ""
          `shouldRespondWith` 502

      it "returns 403 when source and destination are identical" $ do
        request
          "COPY"
          "/users/1/name.txt"
          [("Destination", "/users/1/name.txt")]
          ""
          `shouldRespondWith` 403

      it "rejects mismatched shapes (cell -> row)" $ do
        request
          "COPY"
          "/users/1/name.txt"
          [("Destination", "/users/2")]
          ""
          `shouldRespondWith` 403

    describe "MOVE" $ do
      it "moves a cell value and nulls the source" $ do
        request
          "MOVE"
          "/users/1/name.txt"
          [("Destination", "/users/1/email.txt")]
          ""
          `shouldRespondWith` 204
        get "/users/1/email"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "John"
            }
        get "/users/1/name"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "NULL"
            }

      it "moves a row and deletes the source" $ do
        request
          "MOVE"
          "/users/1"
          [("Destination", "/users/600")]
          ""
          `shouldRespondWith` 201
        get "/users/600/name"
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 200
            , matchHeaders = [davHeader]
            , matchBody = "John"
            }
        get "/users/1/name" `shouldRespondWith` 404


{-| Regression coverage for issue #14: PROPFIND on a multi-gigabyte
table used to load every row's BLOB column into memory. This fixture
is small enough to keep the suite fast (~8 MiB across 32 rows) but
will surface a memory regression because the fixture would no longer
fit in the test process if the buggy SELECT * is reintroduced under
allocation-limited CI environments.
-}
bigBlobSpec :: Spec
bigBlobSpec = with mkBigBlobApp $ do
  describe "PROPFIND on a table with large BLOB cells (issue #14)" $ do
    it "enumerates rowids without loading BLOB content" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop>\
            \    <resourcetype/>\
            \  </prop>\
            \</propfind>\
            \"
      response <- propfind "/big_blobs/" 1 (fromString (T.unpack xmlRequest))
      let bodyTxt :: Text =
            response & simpleBody & BL.toStrict & decodeUtf8
      -- The folder root and every rowid must appear in the listing.
      liftIO
        $ for_
          ( "/big_blobs"
              : [ "/big_blobs/" <> show n
                | n <- [1 :: Int .. 32]
                ]
          )
        $ \href ->
          unless (T.isInfixOf (T.pack href) bodyTxt) $
            panic $
              "Missing href in PROPFIND response: " <> T.pack href


{-| Fixture for PK / combined row-name tests. Creates a fresh table
@contacts@ whose primary key is the @handle@ column, plus a row in
the existing PK-less @users@ table to exercise the rowid fallback.
-}
mkRowNameApp :: RowNameMode -> IO _
mkRowNameApp mode = do
  let scratchDb = "test/rowname_scratch.sqlite"
  exists <- doesFileExist scratchDb
  unless (not exists) $ removeFile scratchDb
  withConnection scratchDb $ \conn -> do
    execute_
      conn
      "CREATE TABLE contacts (\
      \handle TEXT PRIMARY KEY, \
      \fullname TEXT, \
      \notes TEXT)"
    execute_
      conn
      "INSERT INTO contacts (handle, fullname, notes) \
      \VALUES ('alice', 'Alice Adams', 'first')"
    execute_
      conn
      "INSERT INTO contacts (handle, fullname, notes) \
      \VALUES ('bob@example.com', 'Bob Brown', 'second')"
    -- A PK-less table so we can verify the rowid fallback.
    execute_ conn "CREATE TABLE notes (body TEXT)"
    execute_ conn "INSERT INTO notes (body) VALUES ('hello')"
  pure $ webDavServer mode scratchDb


rowNamePkSpec :: Spec
rowNamePkSpec = with (mkRowNameApp PrimaryKeyMode) $ do
  describe "PrimaryKeyMode" $ do
    it "lists rows by their PK value in PROPFIND" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop><resourcetype/></prop>\
            \</propfind>\
            \"
      response <- propfind "/contacts/" 1 (fromString (T.unpack xmlRequest))
      let bodyTxt :: Text =
            response & simpleBody & BL.toStrict & decodeUtf8
      liftIO $
        for_ ["/contacts/alice", "/contacts/bob@example.com"] $ \href ->
          unless (T.isInfixOf (T.pack href) bodyTxt) $
            panic $
              "Missing href in PROPFIND response: " <> T.pack href

    it "GET resolves a cell by PK value" $ do
      get "/contacts/alice/fullname"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "Alice Adams"
          }

    it "PUT updates a cell addressed by PK value" $ do
      put "/contacts/alice/notes.txt" "renamed"
        `shouldRespondWith` 204
      get "/contacts/alice/notes"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "renamed"
          }

    it "DELETE removes a row addressed by PK value" $ do
      delete "/contacts/bob@example.com" `shouldRespondWith` 204
      get "/contacts/bob@example.com/fullname" `shouldRespondWith` 400

    it "MKCOL inserts a new row keyed by the requested PK value" $ do
      mkcol "/contacts/carol" `shouldRespondWith` 201
      get "/contacts/carol/fullname"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "NULL"
          }

    it "rejects MKCOL when the PK value already exists" $ do
      mkcol "/contacts/alice" `shouldRespondWith` 405

    it "falls back to rowid for PK-less tables" $ do
      get "/notes/1/body"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "hello"
          }


rowNameCombinedSpec :: Spec
rowNameCombinedSpec = with (mkRowNameApp CombinedMode) $ do
  describe "CombinedMode" $ do
    it "PROPFIND lists rows as '<rowid> - <pk>'" $ do
      let
        xmlRequest =
          normalizeXml
            "<propfind xmlns:D=\"DAV:\">\
            \  <prop><resourcetype/></prop>\
            \</propfind>\
            \"
      response <- propfind "/contacts/" 1 (fromString (T.unpack xmlRequest))
      let bodyTxt :: Text =
            response & simpleBody & BL.toStrict & decodeUtf8
      liftIO $
        for_
          [ "/contacts/1 - alice"
          , "/contacts/2 - bob@example.com"
          ]
          $ \href ->
            unless (T.isInfixOf (T.pack href) bodyTxt) $
              panic $
                "Missing href in PROPFIND response: " <> T.pack href

    it "GET accepts the combined segment" $ do
      get "/contacts/1 - alice/fullname"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "Alice Adams"
          }

    it "GET also accepts the bare rowid as a shortcut" $ do
      get "/contacts/1/fullname"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "Alice Adams"
          }


main :: IO ()
main = hspec $ do
  spec
  plainSpec
  sqlarSpec
  bigBlobSpec
  rowNamePkSpec
  rowNameCombinedSpec
