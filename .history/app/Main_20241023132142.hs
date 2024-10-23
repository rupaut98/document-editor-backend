module Main where

import Network.Wai.Handler.Warp (run)
import Server (startApp)

-- Main entry point
main :: IO ()
main = startApp
