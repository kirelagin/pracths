module Main where

import Data.Char (isSpace)
import Data.Monoid (Sum (Sum))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified System.Environment as Env
import qualified System.IO as IO


main :: IO ()
main = do
  fp <- (!! 0) <$> Env.getArgs
  run fp


data WordsResult = WordsResult
  { wrStartW :: !Bool
  , wrWords :: {-# UNPACK #-} !Int
  , wrEndW :: !Bool
  }

instance Semigroup WordsResult where
  WordsResult s1 w1 e1 <> WordsResult s2 w2 e2 =
    WordsResult s1 (w1 + w2 - if e1 && s2 then 1 else 0) e2

instance Monoid WordsResult where
  mempty = WordsResult False 0 False


data CountResult = CountResult
  { crLines :: {-# UNPACK #-} !(Sum Int)
  , crWords :: {-# UNPACK #-} !WordsResult
  , crBytes :: {-# UNPACK #-} !(Sum Int)
  }

instance Semigroup CountResult where
  CountResult l1 w1 b1 <> CountResult l2 w2 b2 =
    CountResult (l1 <> l2) (w1 <> w2) (b1 <> b2)

instance Monoid CountResult where
  mempty = CountResult mempty mempty mempty

instance Show CountResult where
  show (CountResult (Sum l) (WordsResult _ w _) (Sum b)) =
    show l <> "\t" <> show w <> "\t" <> show b

run :: String -> IO ()
run fp = IO.withBinaryFile fp IO.ReadMode $ \h -> do
  str <- BSL.hGetContents h
  print $ count str

count :: BSL.ByteString -> CountResult
count s = C8.foldl' go mempty s
  where
    go :: CountResult -> Char -> CountResult
    go prevR c =
      let
        addL | c == '\n' = 1
             | otherwise = 0
        nsp = not $ isSpace c
        addW | nsp = 1
             | otherwise = 0
        newR = CountResult (Sum addL) (WordsResult nsp addW nsp) (Sum 1)
      in prevR <> newR
