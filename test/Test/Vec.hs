module Test.Vec where

import Test.HUnit

import Vec


unit_vecShow :: Assertion
unit_vecShow =
  "Vec{3}[1,2,3]" @=? show (VCons 1 $ VCons 2 $ VCons (3 :: Int) $ VNil)
