module Test.Vec where

import Hedgehog
import Test.HUnit

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Nat
import Vec


unit_vecShow :: Assertion
unit_vecShow =
  "Vec{3}[1,2,3]" @=? show (VCons 1 $ VCons 2 $ VCons (3 :: Int) $ VNil)

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three
type Five = 'Succ Four

hprop_vtake3of4 :: Property
hprop_vtake3of4 = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  let v = VCons a $ VCons b $ VCons c $ VCons d $ VNil
  "Vec{3}[" <> show a <> "," <> show b <> "," <> show c <> "]" === show (vtake @Three v)
