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
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8

import Data.List
import Data.Traversable

import Network.HTTP.Types.URI

import Servant
import Servant.Foreign.Internal

import System.Directory

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options

import Text.XML.Light

import Network.WebDav.Constants
import Network.WebDav.Properties


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

instance MimeRender XML [PropResults] where
  mimeRender _ items = 
    Lazy.Char8.pack $ showTopElement $ 
    e "multistatus"
    [Attr (unqual "xmlns:D") "DAV:"]
    $ map propResultsToXml items 

instance MimeUnrender XML Element where
  mimeUnrender _ x =
    case parseXMLDoc x of
     Nothing -> Left $ "Bad XML Input: " ++ show x
     Just doc -> Right doc



propResultsToXml::PropResults->Element
propResultsToXml PropResults{..} = do
  e "response" [] ([
    te "href" [] $ webBase ++ propName,
    e "propstat" [] [
      te "status" [] "HTTP/1.1 200 OK",
      e "prop" [] (
        (case itemType of
          File -> []
          Folder -> 
            [e "resourcetype" [] [e "collection" [] []]])
        ++
        map (\(name, val) -> te name [] val) props
        )
      ]]
    ++ (
    if length propMissing /= 0
    then 
      [
        e "propstat" [] [
           te "status" [] "HTTP/1.1 404 Not Found",
           e "prop" [] $ map (\x -> e x [] []) propMissing,
           te "responsedescription" [] "Property was not found"
           ]
      ]
    else []))




instance Accept XML where
  contentType _ = "application/xml"
instance NotFound where
  
type UserAPI1 =
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

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = addHeaders [("Dav", "1, 2, ordered-collections")] $ provideOptions userAPI $ logStdoutDev $ serve userAPI
       (
         doMkCol
         :<|> doPropFind
         :<|> doGet
         :<|> doPut
         :<|> doDelete
         :<|> doMove
         :<|> doCopy
       )
      
main :: IO ()
main = run 20001 app1


doMove::[String]->Maybe String->Handler String
doMove _ Nothing = error "Missing 'destination' header"
doMove urlPath (Just destination) = do
  let destination' = Char8.unpack (urlDecode False (Char8.pack destination))
  liftIO $ putStrLn $ "In doMove: " ++ destination'
  let fullPath = "/" ++ intercalate "/" urlPath

  let relativePath =
        if webBase `isPrefixOf` destination'
        then drop (length webBase) destination'
        else error "destination is not on this webdav server"
  
  liftIO $ putStrLn $ "Moving " ++ fullPath ++ " to " ++ relativePath

  liftIO $ renamePath (fileBase ++ fullPath) (fileBase ++ relativePath)
  
  return ""

doCopy::[String]->Maybe String->Handler String
doCopy _ Nothing = error "Missing 'destination' header"
doCopy urlPath (Just destination) = do
  let destination' = Char8.unpack (urlDecode False (Char8.pack destination))
  liftIO $ putStrLn $ "In doMove: " ++ destination'
  let fullPath = "/" ++ intercalate "/" urlPath

  let relativePath =
        if webBase `isPrefixOf` destination'
        then drop (length webBase) destination'
        else error "destination is not on this webdav server"
  
  liftIO $ putStrLn $ "Moving " ++ fullPath ++ " to " ++ relativePath

  liftIO $ copyFile (fileBase ++ fullPath) (fileBase ++ relativePath)
  
  return ""

doPut::[String]->ByteString->Handler String
doPut urlPath body = do
  liftIO $ putStrLn "In doPut"
  let fullPath = "/" ++ intercalate "/" urlPath

  liftIO $ putStrLn $ "Creating file: " ++ fullPath

  liftIO $ ByteString.writeFile (fileBase ++ fullPath) body
  
  return ""

doGet::[String]->Handler String
doGet urlPath = do
  liftIO $ putStrLn "In doGet"
  let fullPath = "/" ++ intercalate "/" urlPath

  liftIO $ putStrLn $ "Getting file: " ++ fullPath

  liftIO $ readFile (fileBase ++ fullPath)

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

doPropFind::[String]->Element->Handler [PropResults]
doPropFind urlPath doc = do
  --TODO - check that the xml path element names are all correct....
  let propNames = [qName $ elName x | Elem x <- concat $ map elContent $ [x | Elem x <- elContent doc]]
  liftIO $ putStrLn $ "In doPropFind: " ++ show propNames
  let relPath = concat $ map ("/" ++) urlPath

  let fullPath=fileBase++relPath
  liftIO $ putStrLn $ "relPath: " ++ relPath
  liftIO $ putStrLn $ "fullPath: " ++ fullPath
  isDir <- liftIO $ doesDirectoryExist fullPath
  isFile <- liftIO $ doesFileExist fullPath

  files <- 
    case (isDir, isFile) of
     (False, False) -> throwError err404
     (False, True) -> return [relPath]
     (True, False) -> do
       fileNames <- liftIO $ listDirectory fullPath
       return $ relPath:(map ((relPath ++ "/") ++) fileNames)
     (True, True) -> error $ "internal logic error, getObject called on object that is both file and dir: " ++ fullPath

  for files $ 
      liftIO . getPropResults propNames
