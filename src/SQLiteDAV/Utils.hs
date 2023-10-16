module SQLiteDAV.Utils where

import Protolude (Text, show, ($))

import Data.Text qualified as T
import Database.SQLite.Simple (SQLData (..))
import Data.ByteString.Lazy.Char8 qualified as Lazy.Char8


sqlDataToText :: SQLData -> Text
sqlDataToText sqlData =
  case sqlData of
    SQLText text -> text
    SQLInteger int -> T.pack $ show int
    SQLFloat float -> T.pack $ show float
    SQLBlob blob -> T.pack $ show blob
    SQLNull -> "NULL"



sqlDataToFileContent :: SQLData -> Lazy.Char8.ByteString
sqlDataToFileContent sqlData =
  case sqlData of
    SQLText text -> Lazy.Char8.pack $ T.unpack text
    SQLInteger int -> show int
    SQLFloat float -> show float
    SQLBlob blob -> Lazy.Char8.fromStrict blob
    SQLNull -> "NULL"
