module Test.Some where

import Test.HUnit

import Some


unit_showVariantTypeBool :: Assertion
unit_showVariantTypeBool =
  "Bool" @=? showVariantType (MkVariant True)

unit_showVariantTypeString :: Assertion
unit_showVariantTypeString =
  "[Char]" @=? showVariantType (MkVariant "Hello!")
