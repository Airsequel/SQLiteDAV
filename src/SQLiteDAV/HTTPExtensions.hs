{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SQLiteDAV.HTTPExtensions where

import Protolude (Either (..), Maybe (..), show, ($), (++))
import Servant (
  Accept (contentType),
  MimeUnrender (mimeUnrender),
  ReflectMethod (..),
  Verb,
 )
import Servant.Foreign.Internal ()
import Text.XML.Light (Element, parseXMLDoc)


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


-- MKCOL returns 201 Created on success per RFC 4918 §9.3.1.
type Mkcol = Verb 'MKCOL 201
type Propfind = Verb 'PROPFIND 207
type Proppatch = Verb 'PROPPATCH 200
type Lock = Verb 'LOCK 200
type Unlock = Verb 'UNLOCK 204
type Orderpatch = Verb 'ORDERPATCH 200


-- type Head = Verb 'HEAD 200
-- type Trace = Verb 'TRACE 200
-- COPY/MOVE return 201 Created when the destination is new (RFC 4918
-- §9.8.5/§9.9.4). Handlers return 204 No Content for overwrites by
-- throwing a tailored ServerError.
type Copy = Verb 'COPY 201
type Move = Verb 'MOVE 201


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
