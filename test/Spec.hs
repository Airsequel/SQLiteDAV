{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Spec where

import Protolude (
  Char,
  IO,
  Integer,
  Maybe (..),
  Text,
  decodeUtf8,
  fmap,
  isSpace,
  liftIO,
  not,
  pure,
  show,
  traceShowId,
  ($),
  (&),
  (.),
  (<&>),
  (<>),
  (==),
 )
import Protolude.Unsafe (unsafeIndex)

import Data.ByteString.Lazy qualified as BL
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Debug.Trace (traceM)
import Network.HTTP.Types (hContentType)
import System.Directory (copyFile)
import Network.Wai ()
import Network.Wai.Test (SResponse (..), simpleBody)
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
import SQLiteDAV.Server (webDavServer)
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
  rmXmlSpace
    $ "<?xml version='1.0' ?>"
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


-- | Copy the test fixture database to a scratch path so tests can mutate it
-- without polluting the committed file.
mkTestApp :: IO _
mkTestApp = do
  let scratchDb = "test/data_scratch.sqlite"
  copyFile "test/data.sqlite" scratchDb
  pure $ webDavServer scratchDb


-- | Same as 'mkTestApp' but for the sqlar fixture.
mkSqlarApp :: IO _
mkSqlarApp = do
  let scratchDb = "test/archive_scratch.sqlar"
  copyFile "test/archive.sqlar" scratchDb
  pure $ webDavServer scratchDb


put :: _ -> _ -> WaiSession st SResponse
put path =
  request
    "PUT"
    path
    [(hContentType, "application/octet-stream")]


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


main :: IO ()
main = hspec $ do
  spec
  plainSpec
  sqlarSpec
