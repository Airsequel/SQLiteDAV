module Main where

import Protolude (IO, ($))

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import SQLiteDAV.Server (webDavServer)


main :: IO ()
main =
  run 20001 $ logStdoutDev webDavServer
