module Main where

import Data.Char (isSpace)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
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
run fp = IO.withBinaryFile fp IO.ReadMode $ \h -> do
  str <- BSL.hGetContents h
  print $ count str

count :: BSL.ByteString -> CountResult
count s = C8.foldl' go (CountResult 0 0 False 0) s
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
