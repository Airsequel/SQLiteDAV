{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
      DataKinds,
      FlexibleInstances,
      MultiParamTypeClasses,
      OverloadedStrings,
      RecordWildCards,
      TypeOperators
#-}

module Network.WebDav.HTTPExtensions where

{-
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8

import Data.List
import Data.Traversable

import Network.HTTP.Types.URI
-}
import Servant
import Servant.Foreign.Internal
{-
import System.Directory

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options
-}
import Text.XML.Light
{-
import Network.WebDav.Constants
import Network.WebDav.Properties
-}

data DavMethod = MKCOL | PROPFIND | PROPPATCH | LOCK | UNLOCK | ORDERPATCH | COPY | MOVE


-- OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, COPY, MOVE

instance ReflectMethod 'MKCOL where
  reflectMethod _ = "MKCOL"
instance ReflectMethod 'PROPFIND where
  reflectMethod _ = "PROPFIND"
instance ReflectMethod 'PROPPATCH where
  reflectMethod _ = "PROPPATCH"
instance ReflectMethod 'LOCK where
  reflectMethod _ = "LOCK"
instance ReflectMethod 'UNLOCK where
  reflectMethod _ = "UNLOCK"
instance ReflectMethod 'ORDERPATCH where
  reflectMethod _ = "ORDERPATCH"
--instance ReflectMethod 'HEAD where
--  reflectMethod _ = "HEAD"
--instance ReflectMethod 'TRACE where
--  reflectMethod _ = "TRACE"
instance ReflectMethod 'COPY where
  reflectMethod _ = "COPY"
instance ReflectMethod 'MOVE where
  reflectMethod _ = "MOVE"



type Mkcol = Verb 'MKCOL 200
type Propfind = Verb 'PROPFIND 207
type Proppatch = Verb 'PROPPATCH 200
type Lock = Verb 'LOCK 200
type Unlock = Verb 'UNLOCK 200
type Orderpatch = Verb 'ORDERPATCH 200
--type Head = Verb 'HEAD 200
--type Trace = Verb 'TRACE 200
type Copy = Verb 'COPY 200
type Move = Verb 'MOVE 200

data XML = XML

instance MimeUnrender XML Element where
  mimeUnrender _ x =
    case parseXMLDoc x of
     Nothing -> Left $ "Bad XML Input: " ++ show x
     Just doc -> Right doc


instance Accept XML where
  contentType _ = "application/xml"
instance NotFound where
  
