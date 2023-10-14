{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module SQLiteDAV.API where

import Data.ByteString (ByteString)

import Protolude (Char)

import Data.Text (Text)
import SQLiteDAV.HTTPExtensions (
  AppXML,
  Copy,
  Mkcol,
  Move,
  Propfind,
  TextXML,
 )
import SQLiteDAV.Properties (PropResults)
import Servant (
  CaptureAll,
  Delete,
  Get,
  Header,
  JSON,
  OctetStream,
  PlainText,
  Proxy (..),
  Put,
  ReqBody,
  StdMethod (OPTIONS),
  Verb,
  type (:<|>),
  type (:>),
 )
import Text.XML.Light (Element)


type String = [Char]


type Options = Verb 'OPTIONS 200


-- TODO: Figure out why JSON is necessary for every endpoint and remove it
type WebDavAPI =
  CaptureAll "segments" String
    :> Mkcol '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Header "Depth" Text
      :> ReqBody '[AppXML, TextXML, JSON] Element
      :> Propfind '[AppXML, TextXML, JSON] [PropResults]
    :<|> CaptureAll "segments" String
      :> Get '[PlainText, JSON] String
    :<|> CaptureAll "segments" String
      :> ReqBody '[OctetStream, JSON] ByteString
      :> Put '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Delete '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Move '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Copy '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Options '[JSON] ()


--  :<|> Proppatch '[JSON] [Int]
--  :<|> Lock '[JSON] [Int]
--  :<|> Unlock '[JSON] [Int]
--  :<|> Orderpatch '[JSON] [Int]
--  :<|> Post '[JSON] [Int]

--  :<|> Head '[JSON] [Int]
--  :<|> Trace '[JSON] [Int]

webDavAPI :: Proxy WebDavAPI
webDavAPI = Proxy
