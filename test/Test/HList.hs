module Test.HList where

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import HList


hprop_show :: Property
hprop_show = property $ do
  a <- forAll $ G.int R.linearBounded
  b <- forAll $ G.string (R.linear 0 100) G.alphaNum
  c <- forAll $ G.bool

  let hl = HCons a $ HCons b $ HCons c $ HNil
  "["<>show a<>","<>show b<>","<>show c<>"]" === show hl
