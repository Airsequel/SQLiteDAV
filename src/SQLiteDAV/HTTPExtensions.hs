{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SQLiteDAV.HTTPExtensions where

import Protolude (
  Bool (..),
  Char,
  Either (..),
  Maybe (..),
  any,
  not,
  otherwise,
  show,
  ($),
  (&&),
  (++),
  (.),
  (==),
  (||),
 )

import Control.Exception (SomeException)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Servant (
  Accept (contentType),
  MimeUnrender (mimeUnrender),
  OctetStream,
  ReflectMethod (..),
  Verb,
 )
import Servant.Foreign.Internal ()
import Text.XML qualified as XC
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


{-| Reject prefixed names bound to an empty namespace URI.

xml-conduit follows Namespaces 1.1's lenient "undeclaration" rule
and quietly produces @nameNamespace = Just ""@ rather than
rejecting the declaration. Per Namespaces 1.0 (which most WebDAV
clients still expect) the binding is invalid, so we walk the
parsed document and refuse it explicitly.
-}
hasEmptyPrefixedNamespace :: XC.Document -> Bool
hasEmptyPrefixedNamespace = checkElement . XC.documentRoot
  where
    checkElement el =
      isBadName (XC.elementName el)
        || any isBadName (Map.keys (XC.elementAttributes el))
        || any checkNode (XC.elementNodes el)
    checkNode (XC.NodeElement el) = checkElement el
    checkNode _ = False
    isBadName n =
      case XC.namePrefix n of
        Just prefix
          | not (T.null prefix) ->
              XC.nameNamespace n == Just T.empty
        _ -> False


{-| Parse the request body as XML.

xml-light is too permissive (it accepts unclosed tags like @<foo>@
as if they were self-closing and ignores invalid namespace
declarations). xml-conduit does proper XML 1.0 parsing, so we use
it as a well-formedness gate. We additionally reject prefixed
names bound to the empty namespace URI to catch the @xmlns:p=""@
case that xml-conduit treats as a prefix undeclaration. Only
bodies that pass both checks are fed to xml-light for the
'Element' representation the handler already consumes.
-}
xmlMimeUnrender :: BL.ByteString -> Either [Char] Element
xmlMimeUnrender bytes =
  case XC.parseLBS XC.def bytes :: Either SomeException XC.Document of
    Left e -> Left $ "Malformed XML: " ++ show e
    Right doc
      | hasEmptyPrefixedNamespace doc ->
          Left "Prefixed name bound to empty namespace URI"
      | otherwise ->
          case parseXMLDoc (TL.decodeUtf8 bytes) of
            Nothing -> Left $ "Could not represent XML as Element"
            Just el -> Right el


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


-- Some clients (notably neon, used by litmus) send a PROPFIND body
-- without a Content-Type header at all. Servant then defaults to
-- @application/octet-stream@; route those bodies through the same
-- XML well-formedness check instead of rejecting with 415.
instance MimeUnrender OctetStream Element where
  mimeUnrender _ = xmlMimeUnrender
