{-# LANGUAGE
      OverloadedStrings
#-}

module Network.WebDav.Server where

import Control.Monad
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
      

doMove::[String]->Maybe String->Handler ()
doMove _ Nothing = error "Missing 'destination' header"
doMove urlPath (Just destination) = liftIO $ do
  fromPath <- getSafePath urlPath
  toPath <- getSafePath2 destination
  renamePath fromPath toPath

doCopy::[String]->Maybe String->Handler ()
doCopy _ Nothing = error "Missing 'destination' header"
doCopy urlPath (Just destination) = liftIO $ do
  fromPath <- getSafePath urlPath
  toPath <- getSafePath2 destination
  copyFile fromPath toPath

doPut::[String]->ByteString->Handler ()
doPut urlPath body = liftIO $ flip ByteString.writeFile body =<< getSafePath urlPath

doGet::[String]->Handler String
doGet urlPath = liftIO $ readFile =<< getSafePath urlPath

doDelete::[String]->Handler ()
doDelete urlPath = liftIO $ removePathForcibly =<< getSafePath urlPath
  
doMkCol::[String]->Handler ()
doMkCol urlPath = liftIO $ createDirectory =<< getSafePath urlPath

doPropFind::[String]->Element->Handler [PropResults]
doPropFind urlPath doc = do
  --TODO - check that the xml path element names are all correct....
  let propNames = [qName $ elName x | Elem x <- concat $ map elContent $ [x | Elem x <- elContent doc]]

  let relPath = concat $ map ("/" ++) urlPath

  let fullPath=fileBase++relPath

  -- partial safety check, see notes below....
  when ("/../" `isInfixOf` fullPath) $ error "Invalid path"

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







-- this function gets the local filepath
-- It is important that the path can not go outside of the webdav folder, so we have to make sure
-- that no double dot paths are in the filepath.
-- Honestly, it looks like servant doesn't allow this anyway, so this check may never trigger.
-- Also, the better way to do this is to normalize the path (remove dots, double slashes, etc),
-- then verify that it fall in the path.  Unfortunately I couldn't find a good pre-written
-- normalization function, so I will just do a simpler comparison for now.
getSafePath::[String]->IO FilePath
getSafePath urlPath = do
  let fullPath = concat $ map ("/" ++) urlPath
  if "/../" `isInfixOf` fullPath
    then error "Invalid path"
    else return $ fileBase ++ fullPath

-- This is a variant of getSafePath for paths given in the headers.
-- I don't think servant checks for dot paths in the header values, so this check is
-- much more important here.
-- This should probably be thought through a bit more.
getSafePath2::String->IO FilePath
getSafePath2 urlPath = do
  let urlPath' = Char8.unpack (urlDecode False (Char8.pack urlPath))

  let relativePath =
        if webBase `isPrefixOf` urlPath'
        then drop (length webBase) urlPath'
        else error "destination is not on this webdav server"

  let fullPath = fileBase ++ relativePath

  if "/../" `isInfixOf` fullPath
    then error "Invalid path"
    else return fullPath
