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


main :: IO ()
main = hspec spec
