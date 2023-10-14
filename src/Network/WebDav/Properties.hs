{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Use list comprehension" #-}

module Network.WebDav.Properties where

import Protolude (
  Char,
  Eq,
  FilePath,
  IO,
  Maybe (..),
  Show,
  Text,
  fmap,
  not,
  null,
  pure,
  putStrLn,
  show,
  ($),
  (++),
  (<&>),
 )

import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as Lazy.Char8
import Data.Either (lefts, rights)
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Network.WebDav.Constants (dbPath, webBase)
import Network.WebDav.HTTPExtensions (AppXML, TextXML)
import Protolude.Error (error)
import Servant (MimeRender (..))
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getModificationTime,
 )
import System.FilePath.Posix (takeExtension, takeFileName)
import System.Posix (fileSize, getFileStatus)
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


type String = [Char]


instance FromJSON ByteString where
  parseJSON = error "FromJSON ByteString not implemented"


instance FromJSON Element where
  parseJSON = error "FromJSON Element not implemented"


data ItemType = File | Folder
  deriving (Show, Eq, Generic)


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
    , elContent = content <&> Elem
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
    $ items <&> propResultsToXml


instance MimeRender AppXML [PropResults] where
  mimeRender _ = xmlMimeRender


instance MimeRender TextXML [PropResults] where
  mimeRender _ = xmlMimeRender


propResultsToXml :: PropResults -> Element
propResultsToXml PropResults{..} = do
  e
    "response"
    []
    ( [ te "href" [] $ "/" ++ propName
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
                  ++ fmap (\(name, val) -> te name [] val) props
              )
          ]
      ]
        ++ ( if not (null propMissing)
              then
                [ e
                    "propstat"
                    []
                    [ te "status" [] "HTTP/1.1 404 Not Found"
                    , e "prop" [] $ fmap (\x -> e x [] []) propMissing
                    , te "responsedescription" [] "Property was not found"
                    ]
                ]
              else []
           )
    )


getProp :: FilePath -> Text -> IO (Maybe Text)
getProp filePath prop = case prop of
  "getlastmodified" -> do
    lastModified <- getModificationTime $ dbPath ++ filePath
    pure $
      Just $
        T.pack $
          formatTime
            defaultTimeLocale
            "%a, %e %b %Y %H:%M:%S %Z"
            lastModified
  "creationdate" -> pure Nothing -- Unix doesn't seem to store creation date
  "displayname" -> pure $ Just $ T.pack $ takeFileName filePath
  "getcontentlength" -> do
    stat <- getFileStatus $ dbPath ++ filePath
    pure $ Just $ show $ fileSize stat
  "getcontenttype" -> do
    case takeExtension filePath of
      ".txt" -> pure $ Just "text/plain"
      _ -> pure Nothing
  "resourcetype" -> pure Nothing -- this is handled elsewhere
  _ -> do
    putStrLn $
      "Warning: Server requested a property \
      \that we do not handle: "
        ++ T.unpack prop
    pure Nothing
