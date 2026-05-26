{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module SQLiteDAV.API where

import Protolude (Char, Integer, Maybe (..), Show, (<>))

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Database.SQLite.Simple (SQLData)
import Servant (
  CaptureAll,
  Get,
  Header,
  Headers,
  JSON,
  NoContent,
  OctetStream,
  PlainText,
  Proxy (..),
  ReqBody,
  StdMethod (DELETE, OPTIONS, PUT),
  Verb,
  type (:<|>),
  type (:>),
 )
import Servant.API.ContentTypes (AllCTRender, AllMime, handleAcceptH)
import Text.XML.Light (Element)

import SQLiteDAV.HTTPExtensions (
  AppXML,
  Copy,
  Lock,
  Mkcol,
  Move,
  Propfind,
  TextXML,
  Unlock,
 )
import SQLiteDAV.Properties (LockResult, PropResults)
import SQLiteDAV.Utils (sqlDataToFileContent)


type String = [Char]


type Options = Verb 'OPTIONS 200


type Delete = Verb 'DELETE 204


data WithContentType = WithContentType
  { header :: BL.ByteString
  , content :: SQLData
  }
  deriving (Show)


instance AllCTRender '[OctetStream] WithContentType where
  handleAcceptH _ _ (WithContentType header content) =
    Just (header, sqlDataToFileContent content)


instance AllCTRender '[] NoContent where
  handleAcceptH _ _ _ = Nothing


-- `PlainText` content types appear throughout to avoid 406 Not
-- Acceptable for clients that send an Accept header.
type WebDavAPI =
  CaptureAll "segments" String
    -- MKCOL: RFC 4918 §9.3 — body must be empty, hence the Content-Length
    -- header guard in the handler.
    :> Header "Content-Length" Integer
    :> Mkcol '[PlainText] NoContent
    :<|> CaptureAll "segments" String
      :> Header "Depth" Text
      :> Header "Prefer" Text
      :> ReqBody '[AppXML, TextXML] Element
      :> Propfind
           '[AppXML, TextXML]
           (Headers '[Header "Preference-Applied" String] [PropResults])
    :<|> CaptureAll "segments" String
      :> Get '[OctetStream] WithContentType
    :<|> CaptureAll "segments" String
      :> ReqBody '[OctetStream] ByteString
      -- PUT returns 201 Created on success (RFC 4918 §9.7).
      :> Verb 'PUT 201 '[PlainText] NoContent
    :<|> CaptureAll "segments" String
      :> Delete '[PlainText] NoContent
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Header "Overwrite" String
      :> Move '[PlainText] NoContent
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Header "Overwrite" String
      :> Header "Depth" String
      :> Copy '[PlainText] NoContent
    :<|> CaptureAll "segments" String
      :> Lock
           '[AppXML, TextXML]
           (Headers '[Header "Lock-Token" String] LockResult)
    :<|> CaptureAll "segments" String
      :> Header "Lock-Token" String
      :> Unlock '[PlainText] NoContent
    :<|> CaptureAll "segments" String
      :> Options '[PlainText] NoContent


--  :<|> Proppatch '[JSON] [Int]
--  :<|> Orderpatch '[JSON] [Int]
--  :<|> Post '[JSON] [Int]

--  :<|> Head '[JSON] [Int]
--  :<|> Trace '[JSON] [Int]

webDavAPI :: Proxy WebDavAPI
webDavAPI = Proxy
