module Main where

import Data.Char (isSpace)
import Data.List (foldl')

import qualified System.Environment as Env
import qualified System.IO as IO


main :: IO ()
main = do
  fp <- (!! 0) <$> Env.getArgs
  run fp


data CountResult = CountResult
  { crLines :: !Int
  , crWords :: !Int
  , crPrevW :: !Bool
  , crBytes :: !Int
  }
  deriving Show

run :: String -> IO ()
run fp = IO.withFile fp IO.ReadMode $ \h -> do
  str <- IO.hGetContents h
  print $ count str

count :: String -> CountResult
count s = foldl' go (CountResult 0 0 False 0) s
  where
    go :: CountResult -> Char -> CountResult
    go (CountResult ls wc prevW bs) c =
      let
        addL | c == '\n' = 1
             | otherwise = 0
        sp = isSpace c
        addW | not sp && prevW = 1
             | otherwise = 0
      in CountResult (ls + addL) (wc + addW) (not sp) (bs + 1)
