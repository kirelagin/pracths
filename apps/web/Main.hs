module Main where

import Network.Wai.Handler.Warp (run)

import Web.Server (app)

main :: IO ()
main = run 8081 app
