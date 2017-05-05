{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
      DataKinds,
      FlexibleInstances,
      MultiParamTypeClasses,
      OverloadedStrings,
      RecordWildCards,
      TypeOperators
#-}

module Network.WebDav.Properties where

{-
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
-}
{-
import Data.Either
import Data.List
-}
import Data.Time.Format
{-
import Data.Traversable

import GHC.Stack

import Network.HTTP.Types.URI

import Servant
import Servant.Foreign.Internal
-}
import System.Directory
{-

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Servant.Options

import Text.XML.Light
-}

import Network.WebDav.Constants



data ItemType = File | Folder

data PropResults =
  PropResults {
    propName::String,
    itemType::ItemType,
    props::[(String, String)],
    propMissing::[String]
    }
  
            

getProp::FilePath->String->IO (Maybe String)
getProp filePath "getlastmodified" = do
  lastModified <- getModificationTime $ fileBase ++ filePath
  return $ Just $ formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %Z" lastModified
  
getProp _ _ = return Nothing
