module Main where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Server

startupText :: String
startupText = "Hello from the Ladder Billiards API Server!"

main :: IO ()
main = do
  print startupText
  app <- application
  run 8081 app
