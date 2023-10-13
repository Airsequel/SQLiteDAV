{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Network.WebDav.API where

import Data.ByteString (ByteString)

import Network.WebDav.HTTPExtensions (Copy, Mkcol, Move, Propfind, XML)
import Network.WebDav.Properties
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


type WebDavAPI =
  CaptureAll "segments" String
    :> Mkcol '[JSON] ()
    :<|> CaptureAll "segments" String
      :> ReqBody '[JSON, XML] Element
      :> Propfind '[JSON, XML] [PropResults]
    :<|> CaptureAll "segments" String
      :> Get '[JSON, PlainText] String
    :<|> CaptureAll "segments" String
      :> ReqBody '[JSON, OctetStream] ByteString
      :> Put '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Delete '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Move '[JSON] ()
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Copy '[JSON] ()


--  :<|> Proppatch '[JSON] [Int]
--  :<|> Lock '[JSON] [Int]
--  :<|> Unlock '[JSON] [Int]
--  :<|> Orderpatch '[JSON] [Int]
--  :<|> Post '[JSON] [Int]

--  :<|> Head '[JSON] [Int]
--  :<|> Trace '[JSON] [Int]

webDavAPI :: Proxy WebDavAPI
webDavAPI = Proxy
