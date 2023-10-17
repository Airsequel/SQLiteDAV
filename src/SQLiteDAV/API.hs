{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module SQLiteDAV.API where

import Protolude (Char, Maybe (..), Show, (<>))

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Database.SQLite.Simple (SQLData)
import Servant (
  CaptureAll,
  Delete,
  Get,
  Header,
  JSON,
  NoContent,
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
import Servant.API.ContentTypes (AllCTRender, AllMime, handleAcceptH)
import Text.XML.Light (Element)

import SQLiteDAV.HTTPExtensions (
  AppXML,
  Copy,
  Mkcol,
  Move,
  Propfind,
  TextXML,
 )
import SQLiteDAV.Properties (PropResults)
import SQLiteDAV.Utils (sqlDataToFileContent)


type String = [Char]


type Options = Verb 'OPTIONS 200


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


-- TODO: Figure out why JSON is necessary for every endpoint and remove it
type WebDavAPI =
  CaptureAll "segments" String
    :> Mkcol '[] NoContent
    :<|> CaptureAll "segments" String
      :> Header "Depth" Text
      :> ReqBody '[AppXML, TextXML] Element
      :> Propfind '[AppXML, TextXML] [PropResults]
    :<|> CaptureAll "segments" String
      :> Get '[OctetStream] WithContentType
    :<|> CaptureAll "segments" String
      :> ReqBody '[OctetStream] ByteString
      :> Put '[] NoContent
    :<|> CaptureAll "segments" String
      :> Delete '[] NoContent
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Move '[] NoContent
    :<|> CaptureAll "segments" String
      :> Header "Destination" String
      :> Copy '[] NoContent
    :<|> CaptureAll "segments" String
      -- `PlainText` is necessary to not return 406 errors
      :> Options '[PlainText] NoContent


--  :<|> Proppatch '[JSON] [Int]
--  :<|> Lock '[JSON] [Int]
--  :<|> Unlock '[JSON] [Int]
--  :<|> Orderpatch '[JSON] [Int]
--  :<|> Post '[JSON] [Int]

--  :<|> Head '[JSON] [Int]
--  :<|> Trace '[JSON] [Int]

webDavAPI :: Proxy WebDavAPI
webDavAPI = Proxy
