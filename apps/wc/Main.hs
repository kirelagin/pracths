module Main where

import qualified System.Environment as Env
import qualified System.IO as IO


main :: IO ()
main = do
  fp <- (!! 0) <$> Env.getArgs
  run fp


type CountResult = (Int, Int, Int)

run :: String -> IO ()
run fp = IO.withFile fp IO.ReadMode $ \h -> do
  str <- IO.hGetContents h
  print $ count str

count :: String -> CountResult
count s = (length $ lines s, length $ words s, length s)
