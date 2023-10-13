{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.WebDav.HTTPExtensions where

import Servant
import Servant.Foreign.Internal
import Text.XML.Light


data DavMethod
  = MKCOL
  | PROPFIND
  | PROPPATCH
  | LOCK
  | UNLOCK
  | ORDERPATCH
  | COPY
  | MOVE


-- OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, COPY, MOVE
-- are already defined by Servant

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


-- instance ReflectMethod 'HEAD where
--  reflectMethod _ = "HEAD"
-- instance ReflectMethod 'TRACE where
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


-- type Head = Verb 'HEAD 200
-- type Trace = Verb 'TRACE 200
type Copy = Verb 'COPY 200
type Move = Verb 'MOVE 200


data AppXML = AppXML


xmlMimeUnrender x =
  case parseXMLDoc x of
    Nothing -> Left $ "Bad XML Input: " ++ show x
    Just doc -> Right doc


instance MimeUnrender AppXML Element where
  mimeUnrender _ = xmlMimeUnrender


instance Accept AppXML where
  contentType _ = "application/xml"


-- Used by macOS's Finder
data TextXML = TextXML


instance MimeUnrender TextXML Element where
  mimeUnrender _ = xmlMimeUnrender


instance Accept TextXML where
  contentType _ = "text/xml"
