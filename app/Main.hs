module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Server

main :: IO ()
main = do
  app <- application
  run 8081 app
