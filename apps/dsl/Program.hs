{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-unused-do-bind #-}

module Program where

import DSL.Syntax


test :: Syn repr => repr '[] _
test = do
  #x =: 1
  #y =: #x + 2
  print $ #y + 1
  #z =: "hello"
  print $ #z
