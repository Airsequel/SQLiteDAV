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


spec :: Spec
spec = with (pure $ webDavServer "test/data.sqlite") $ do
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
            \    <href>/users/1/photo</href>\
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

  describe "GET" $ do
    it "returns the content of a cell" $ do
      get "/users/1/name"
        `shouldRespondWith` ResponseMatcher
          { matchStatus = 200
          , matchHeaders = [davHeader]
          , matchBody = "John"
          }


main :: IO ()
main = hspec spec
