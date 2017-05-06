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
  CaptureAll "segments" String :> Mkcol '[JSON] String
  :<|> CaptureAll "segments" String :> ReqBody '[XML] Element :> Propfind '[XML] [PropResults]
  :<|> CaptureAll "segments" String :> Get '[PlainText] String
  :<|> CaptureAll "segments" String :> ReqBody '[OctetStream] ByteString :> Put '[JSON] String
  :<|> CaptureAll "segments" String :> Delete '[JSON] String
  :<|> CaptureAll "segments" String :> Header "Destination" String :> Move '[JSON] String
  :<|> CaptureAll "segments" String :> Header "Destination" String :> Copy '[JSON] String
  
--  :<|> Proppatch '[JSON] [Int]
--  :<|> Lock '[JSON] [Int]
--  :<|> Unlock '[JSON] [Int]
--  :<|> Orderpatch '[JSON] [Int]
--  :<|> Post '[JSON] [Int]
  
--  :<|> Head '[JSON] [Int]
--  :<|> Trace '[JSON] [Int]

webDavAPI :: Proxy WebDavAPI
webDavAPI = Proxy
