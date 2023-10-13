module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.WebDav.Server (webDavServer)


main :: IO ()
main =
  run 20001 $ logStdoutDev webDavServer
