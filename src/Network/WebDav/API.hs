{-# LANGUAGE
      DataKinds,
      FlexibleInstances,
      MultiParamTypeClasses,
      OverloadedStrings,
      RecordWildCards,
      TypeOperators
#-}

module Network.WebDav.API where

import Data.ByteString (ByteString)


import Servant


import Text.XML.Light

import Network.WebDav.HTTPExtensions
import Network.WebDav.Properties


type WebDavAPI =
  CaptureAll "segments" String :> Mkcol '[JSON] ()
  :<|> CaptureAll "segments" String :> ReqBody '[XML] Element :> Propfind '[XML] [PropResults]
  :<|> CaptureAll "segments" String :> Get '[PlainText] String
  :<|> CaptureAll "segments" String :> ReqBody '[OctetStream] ByteString :> Put '[JSON] ()
  :<|> CaptureAll "segments" String :> Delete '[JSON] ()
  :<|> CaptureAll "segments" String :> Header "Destination" String :> Move '[JSON] ()
  :<|> CaptureAll "segments" String :> Header "Destination" String :> Copy '[JSON] ()
  
--  :<|> Proppatch '[JSON] [Int]
--  :<|> Lock '[JSON] [Int]
--  :<|> Unlock '[JSON] [Int]
--  :<|> Orderpatch '[JSON] [Int]
--  :<|> Post '[JSON] [Int]
  
--  :<|> Head '[JSON] [Int]
--  :<|> Trace '[JSON] [Int]

webDavAPI :: Proxy WebDavAPI
webDavAPI = Proxy
