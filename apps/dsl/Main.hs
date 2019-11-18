module Main where

import Control.Monad (forM_)

import DSL.Eval (runProgram, showProgram)

import Program (test)

main :: IO ()
main = do
  forM_ (runProgram test) putStrLn
  putStrLn "\n\n----\n\n"
  putStrLn (showProgram test)
