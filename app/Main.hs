{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
      DataKinds,
      FlexibleInstances,
      MultiParamTypeClasses,
      OverloadedStrings,
      RecordWildCards,
      TypeOperators
#-}

module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import Data.DateTime

import Data.List
import Data.Traversable

import GHC.Stack

import Servant
import Servant.Foreign.Internal

import System.Directory
import System.FilePath.Posix

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options

import Text.XML.Light

fileBase::FilePath
fileBase="/home/jim/webdav"

webBase::String
webBase="http://127.0.0.1:20001"

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

e::String->[Attr]->[Element]->Element
e name attrs content = Element{elName=QName{qName=name,qURI=Nothing,qPrefix=Just "D"}, elAttribs=attrs, elContent = map Elem content, elLine=Nothing}

te::String->[Attr]->String->Element
te name attrs text = Element{elName=QName{qName=name,qURI=Nothing,qPrefix=Just "D"}, elAttribs=attrs, elContent = [Text $ CData CDataText text Nothing], elLine=Nothing}

instance MimeRender XML [Int] where
  mimeRender _ _ =
    Lazy.Char8.pack $ showTopElement $ 
    e "multistatus"
    [Attr (unqual "xmlns:D") "DAV:"]
    [folder "/", folder "/aFolder", file "/aFile"]

instance MimeRender XML [FSObject] where
  mimeRender _ items =
    Lazy.Char8.pack $ showTopElement $ 
    e "multistatus"
    [Attr (unqual "xmlns:D") "DAV:"]
    $ map fsObjectToXml items


folder::String->Element
folder url = 
  e "response" [] [
    te "href" [] $ webBase ++ url,
    e "propstat" [] [
      te "status" [] "HTTP/1.1 200 OK",
      e "prop" [] [
        e "resourcetype" [] [
           e "collection" [] []
           ],
        te "getcontentlength" [] "10"]
      ]
    ]

file::String->Element
file url = 
  e "response" [] [
    te "href" [] $ webBase ++ url,
    e "propstat" [] [
      te "status" [] "HTTP/1.1 200 OK",
      e "prop" [] [
        te "creationdate" [] "2017-04-27T08:33:10Z",
        te "displayname" [] "aFile",
        te "getlastmodified" [] "Thu, 27 Apr 2017 08:33:10 GMT",
        e "resourcetype" [] [],
        te "getcontentlength" [] "10"
        ]
      ]
    ]

data FSObject =
  File {
    relativePath::String,
    creationDate::DateTime,
    lastModified::DateTime,
    --resourcetype::??
    getcontentlength::Int
    }
  | Folder {
    relativePath::String,
    resourceType::String,
    contentLength::Int
    }

getFileObject::FilePath->IO FSObject
getFileObject filePath =
  return
  File {
    relativePath=filePath,
    creationDate=fromSeconds 0,
    lastModified=fromSeconds 0,
    --resourcetype::??
    getcontentlength=10
    }

fsObjectToXml::FSObject->Element
fsObjectToXml File{..} =
  e "response" [] [
    te "href" [] $ webBase ++ relativePath,
    e "propstat" [] [
      te "status" [] "HTTP/1.1 200 OK",
      e "prop" [] [
        te "creationdate" [] "2017-04-27T08:33:10Z",
        te "displayname" [] $ takeFileName relativePath,
        te "getlastmodified" [] "Thu, 27 Apr 2017 08:33:10 GMT",
        e "resourcetype" [] [],
        te "getcontentlength" [] "10"
        ]
      ]
    ]
fsObjectToXml Folder{..} =
  e "response" [] [
    te "href" [] $ webBase ++ relativePath,
    e "propstat" [] [
      te "status" [] "HTTP/1.1 200 OK",
      e "prop" [] [
        e "resourcetype" [] [
           e "collection" [] []
           ],
        te "getcontentlength" [] "10"]
      ]
    ]

getFolderObject::FilePath->IO FSObject
getFolderObject filePath =
  return
  Folder {
    relativePath=filePath,
    resourceType="",
    contentLength=10
    }


getObject::HasCallStack=>FilePath->IO FSObject
getObject filePath = do
  let fullPath=fileBase++filePath
  isDir <- doesDirectoryExist fullPath
  isFile <- doesFileExist fullPath
  case (isDir, isFile) of
   (False, False) -> error $ "getObject called on an object that doesn't exist: " ++ fullPath
   (False, True) -> getFileObject filePath
   (True, False) -> getFolderObject filePath
   (True, True) -> error $ "internal logic error, getObject called on object that is both file and dir: " ++ fullPath



  
{-
<?xml version="1.0" encoding="utf-8"?>
<d:multistatus xmlns:d="DAV:">
  <d:response>
    <d:href>http://www.ajaxfilebrowser.com/</d:href>
    <d:propstat>
      <d:status>HTTP/1.1 200 OK</d:status>
      <d:prop>
        <d:creationdate>2014-02-26T01:53:16Z</d:creationdate>
        <d:displayname>Storage</d:displayname>
        <d:getlastmodified>Thu, 27 Apr 2017 08:33:10 GMT</d:getlastmodified>
        <d:resourcetype><d:collection /></d:resourcetype>
      </d:prop>
    </d:propstat>
    <d:propstat>
      <d:status>HTTP/1.1 404 Not Found</d:status>
      <d:prop>
        <d:getcontentlength />
        <d:getcontenttype />
        <d:getetag />
      </d:prop>
      <d:responsedescription>Property was not found</d:responsedescription>
    </d:propstat>
  </d:response>
  <d:response>
    <d:href>http://www.ajaxfilebrowser.com/qqqq/</d:href>
    <d:propstat>
      <d:status>HTTP/1.1 200 OK</d:status>
      <d:prop>
        <d:creationdate>2017-04-27T08:33:10Z</d:creationdate>
        <d:displayname>qqqq</d:displayname>
        <d:getlastmodified>Thu, 27 Apr 2017 08:33:10 GMT</d:getlastmodified>
        <d:resourcetype><d:collection /></d:resourcetype>
      </d:prop>
    </d:propstat>
    <d:propstat>
      <d:status>HTTP/1.1 404 Not Found</d:status>
      <d:prop>
        <d:getcontentlength />
        <d:getcontenttype />
        <d:getetag />
      </d:prop>
      <d:responsedescription>Property was not found</d:responsedescription>
    </d:propstat>
  </d:response>
</d:multistatus>
-}

{-    
<?xml version="1.0" encoding="utf-8"?>
<d:multistatus xmlns:d="DAV:">
  <d:response>
    <d:href>http://www.ajaxfilebrowser.com/Untitled%20Document</d:href>
    <d:propstat>
      <d:status>HTTP/1.1 200 OK</d:status>
      <d:prop>
        <d:creationdate>2017-04-29T23:18:05Z</d:creationdate>
        <d:displayname>Untitled Document</d:displayname>
        <d:getcontentlength>5</d:getcontentlength>
        <d:getcontenttype>application/octet-stream</d:getcontenttype>
        <d:getetag>"4/29/2017 11:18:05 PM-1"</d:getetag>
        <d:getlastmodified>Sat, 29 Apr 2017 23:18:05 GMT</d:getlastmodified>
        <d:resourcetype />
      </d:prop>
    </d:propstat>
  </d:response>
</d:multistatus>
-}

instance Accept XML where
  contentType _ = "text/xml"
instance NotFound where
  
type UserAPI1 =
  CaptureAll "segments" String :> Mkcol '[JSON] String
  :<|> CaptureAll "segments" String :> Propfind '[XML] [FSObject]
  :<|> Proppatch '[JSON] [Int]
  :<|> Lock '[JSON] [Int]
  :<|> Unlock '[JSON] [Int]
  :<|> Orderpatch '[JSON] [Int]
  :<|> Get '[PlainText] String
--  :<|> Head '[JSON] [Int]
  :<|> Post '[JSON] [Int]
  :<|> CaptureAll "segments" String :> Put '[JSON] String
  :<|> CaptureAll "segments" String :> Delete '[JSON] String
--  :<|> Trace '[JSON] [Int]
  :<|> Copy '[JSON] [Int]
  :<|> CaptureAll "segments" String :> Header "Destination" String :> Move '[JSON] String
  :<|> "users" :> Propfind '[JSON] [Int]
  :<|> "users" :> Get '[JSON] [Int]

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = addHeaders [("Dav", "1, 2, ordered-collections")] $ provideOptions userAPI $ logStdoutDev $ serve userAPI
       (
         doMkCol
         :<|> doPropFind
         :<|> server1
         :<|> server1
         :<|> server1
         :<|> server1
         :<|> server2
         :<|> server1
         :<|> doPut
         :<|> doDelete
         :<|> server1
         :<|> doMove
         :<|> server1
         :<|> server1
       )
      
main :: IO ()
main = run 20001 app1


server1::Handler [Int]
server1 = return users1

server2::Handler String
server2 = return "qq"

doMove::[String]->Maybe String->Handler String
doMove _ Nothing = error "Missing 'destination' header"
doMove urlPath (Just destination) = do
  liftIO $ putStrLn "In doMove"
  let fullPath = "/" ++ intercalate "/" urlPath

  let relativePath =
        if webBase `isPrefixOf` destination
        then drop (length webBase) destination
        else error "destination is not on this webdav server"
  
  liftIO $ putStrLn $ "Moving " ++ fullPath ++ " to " ++ relativePath

  liftIO $ renamePath (fileBase ++ fullPath) (fileBase ++ relativePath)
  
  return ""

doPut::[String]->Handler String
doPut urlPath = do
  liftIO $ putStrLn "In doPut"
  let fullPath = "/" ++ intercalate "/" urlPath

  liftIO $ putStrLn $ "Creating file: " ++ fullPath

  liftIO $ writeFile (fileBase ++ fullPath) "qq"
  
  return ""

doDelete::[String]->Handler String
doDelete urlPath = do
  liftIO $ putStrLn "In doDelete"
  let fullPath = "/" ++ intercalate "/" urlPath
  
  liftIO $ removePathForcibly $ fileBase ++ fullPath
  return ""

  
doMkCol::[String]->Handler String
doMkCol urlPath = do
  liftIO $ putStrLn "In doMkCol"
  let fullPath = "/" ++ intercalate "/" urlPath
  liftIO $ print fullPath
  liftIO $ createDirectory $ fileBase ++ fullPath
  return ""
  
doPropFind::[String]->Handler [FSObject]
doPropFind urlPath = do
  liftIO $ putStrLn "In doPropFind"
  let fullPath = "/" ++ intercalate "/" urlPath

  object <- liftIO $ getObject fullPath

  case object of
   File _ _ _ _ -> return [object]
   Folder _ _ _ -> do
     fileNames <- liftIO $ listDirectory $ fileBase ++ fullPath

     objects <- liftIO $ 
                for (map ((fullPath ++ "/") ++) fileNames) getObject

     currentDir <- liftIO $ getFolderObject fullPath
    
     return $ currentDir:objects

users1::[Int]
users1 = [1,2,3,4]
