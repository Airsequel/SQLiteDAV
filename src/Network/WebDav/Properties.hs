{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Use list comprehension" #-}

module Network.WebDav.Properties where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as Lazy.Char8
import Data.Either
import Data.Time.Format
import Data.Traversable
import GHC.Generics
import Network.WebDav.Constants
import Network.WebDav.HTTPExtensions
import Servant
import System.Directory
import System.FilePath.Posix
import System.Posix
import Text.XML.Light (
  Attr (Attr),
  CData (CData),
  CDataKind (CDataText),
  Content (Elem, Text),
  Element (Element, elAttribs, elContent, elLine, elName),
  QName (QName, qName, qPrefix, qURI),
  showTopElement,
  unqual,
 )


instance FromJSON ByteString where
  parseJSON = error "FromJSON ByteString not implemented"


instance FromJSON Element where
  parseJSON = error "FromJSON Element not implemented"


data ItemType = File | Folder
  deriving (Show, Generic)


instance ToJSON ItemType


data PropResults = PropResults
  { propName :: String
  , itemType :: ItemType
  , props :: [(String, String)]
  , propMissing :: [String]
  }
  deriving (Show, Generic)


instance ToJSON PropResults


e :: String -> [Attr] -> [Element] -> Element
e name attrs content =
  Element
    { elName =
        QName
          { qName = name
          , qURI = Nothing
          , qPrefix = Just "D"
          }
    , elAttribs = attrs
    , elContent = map Elem content
    , elLine = Nothing
    }


te :: String -> [Attr] -> String -> Element
te name attrs text =
  Element
    { elName = QName{qName = name, qURI = Nothing, qPrefix = Just "D"}
    , elAttribs = attrs
    , elContent = [Text $ CData CDataText text Nothing]
    , elLine = Nothing
    }


xmlMimeRender :: [PropResults] -> Lazy.Char8.ByteString
xmlMimeRender items =
  Lazy.Char8.pack
    $ showTopElement
    $ e
      "multistatus"
      [Attr (unqual "xmlns:D") "DAV:"]
    $ map propResultsToXml items


instance MimeRender AppXML [PropResults] where
  mimeRender _ = xmlMimeRender


instance MimeRender TextXML [PropResults] where
  mimeRender _ = xmlMimeRender


propResultsToXml :: PropResults -> Element
propResultsToXml PropResults{..} = do
  e
    "response"
    []
    ( [ te "href" [] $ webBase ++ propName
      , e
          "propstat"
          []
          [ te "status" [] "HTTP/1.1 200 OK"
          , e
              "prop"
              []
              ( ( case itemType of
                    File -> []
                    Folder ->
                      [e "resourcetype" [] [e "collection" [] []]]
                )
                  ++ map (\(name, val) -> te name [] val) props
              )
          ]
      ]
        ++ ( if not (null propMissing)
              then
                [ e
                    "propstat"
                    []
                    [ te "status" [] "HTTP/1.1 404 Not Found"
                    , e "prop" [] $ map (\x -> e x [] []) propMissing
                    , te "responsedescription" [] "Property was not found"
                    ]
                ]
              else []
           )
    )


getPropResults :: [String] -> FilePath -> IO PropResults
getPropResults propNames filePath = do
  let fullPath = fileBase ++ filePath
  isDir <- doesDirectoryExist fullPath
  isFile <- doesFileExist fullPath
  let theType =
        case (isDir, isFile) of
          (False, False) -> error $ "file doesn't exist: " ++ show fullPath
          (False, True) -> File
          (True, False) -> Folder
          (True, True) ->
            error $
              "internal logic error, getObject called on object \
              \that is both file and dir: "
                ++ fullPath

  results <-
    for propNames $ \propName -> do
      result <- getProp filePath propName
      case result of
        Nothing -> return $ Left propName
        Just x -> return $ Right (propName, x)

  return
    PropResults
      { propName = filePath
      , itemType = theType
      , props = rights results
      , propMissing = lefts results
      }


getProp :: FilePath -> String -> IO (Maybe String)
getProp filePath "getlastmodified" = do
  lastModified <- getModificationTime $ fileBase ++ filePath
  return $
    Just $
      formatTime
        defaultTimeLocale
        "%a, %e %b %Y %H:%M:%S %Z"
        lastModified
getProp _ "creationdate" =
  return Nothing -- Unix doesn't seem to store creation date
getProp filePath "displayname" = return $ Just $ takeFileName filePath
getProp filePath "getcontentlength" = do
  stat <- getFileStatus $ fileBase ++ filePath
  return $ Just $ show $ fileSize stat
getProp filePath "getcontenttype" = do
  case takeExtension filePath of
    ".txt" -> return $ Just "text/plain"
    _ -> return Nothing
getProp _ "resourcetype" = return Nothing -- this is handled elsewhere
getProp _ prop = do
  putStrLn $
    "Warning: server requested a property that we do not handle: " ++ prop
  return Nothing
