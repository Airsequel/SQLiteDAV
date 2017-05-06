{-# LANGUAGE
      OverloadedStrings
#-}

module Network.WebDav.Server where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.List
import Data.Traversable
import Network.HTTP.Types.URI
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.Servant.Options
import Servant
import System.Directory
import Text.XML.Light



import Network.WebDav.API
import Network.WebDav.Constants
import Network.WebDav.Properties



webDavServer :: Application
webDavServer = addHeaders [("Dav", "1, 2, ordered-collections")] $ provideOptions webDavAPI $ serve webDavAPI
       (
         doMkCol
         :<|> doPropFind
         :<|> doGet
         :<|> doPut
         :<|> doDelete
         :<|> doMove
         :<|> doCopy
       )
      

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
