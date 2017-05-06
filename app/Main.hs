
module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger hiding (destination)
import Network.WebDav.Server
      
main :: IO ()
main = run 20001 $ logStdoutDev $ webDavServer

