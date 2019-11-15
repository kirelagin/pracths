module Test.Some where

import Test.HUnit

import Data.Function ((&))

import Some
import qualified Some as PM


unit_showVariantTypeBool :: Assertion
unit_showVariantTypeBool =
  "Bool" @=? showVariantType (MkVariant True)

unit_showVariantTypeString :: Assertion
unit_showVariantTypeString =
  "[Char]" @=? showVariantType (MkVariant "Hello!")



-- unit_PolyMap :: Assertion
-- unit_PolyMap = do
--   let
--     m = PM.empty
--       & PM.insert "hello" "world"
--       & PM.insert "foo" "bar"
--       & PM.insert True False
-- 
--   Just "world" @=? PM.lookup "hello" m
--   Just "bar" @=? PM.lookup "foo" m
--   Nothing @=? PM.lookup "ouch" m
-- 
--   Just False @=? PM.lookup True m
--   Nothing @=? PM.lookup False m
