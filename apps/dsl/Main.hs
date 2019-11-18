module Main where

import Control.Monad (forM_)

import DSL.AST (showProgram)
import DSL.Eval (runProgram)

import Program (test)

main :: IO ()
main = do
  forM_ (runProgram test) putStrLn
  putStrLn "\n\n----\n\n"
  putStrLn (showProgram test)
