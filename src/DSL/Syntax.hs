{-# OPTIONS_GHC -Wno-orphans #-}

module DSL.Syntax
  ( Var (..)
  , ExprSyn (..)
  , Syn (..)

  , fromInteger
  , fromString
  , return

  , module R
  ) where

import Prelude hiding (return, (>>), print, fromInteger)

import Prelude as R (($))

import Data.Kind (Type)
import GHC.OverloadedLabels as R (IsLabel (fromLabel))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import DSL.Env (VarTypeMap)

import qualified DSL.Env as E


data Var (s :: Symbol) = Var

instance KnownSymbol s => Show (Var s) where
  show Var = symbolVal (Var :: Var s)

instance s1 ~ s => IsLabel s1 (Var s) where
  fromLabel = Var @s


class ExprSyn repr where
  lit :: t -> repr t map
  lkp :: (E.Lookup s t map, KnownSymbol s) => Var s -> repr t map

  (+) :: repr Integer map -> repr Integer map -> repr Integer map
  (-) :: repr Integer map -> repr Integer map -> repr Integer map
  (*) :: repr Integer map -> repr Integer map -> repr Integer map
  abs :: repr Integer map -> repr Integer map
  sgn :: repr Integer map -> repr Integer map

  len :: repr String map -> repr Integer map

fromInteger :: ExprSyn repr => Integer -> repr Integer map
fromInteger = lit

fromString :: ExprSyn repr => String -> repr String map
fromString = lit

instance (E.Lookup s t map, KnownSymbol s, ExprSyn repr) => IsLabel s (repr t map) where
  fromLabel = lkp $ Var @s


class ExprSyn (ExprRepr repr) => Syn repr where
  type ExprRepr repr :: Type -> VarTypeMap -> Type
  (>>) :: repr old map -> repr map new -> repr old new
  (=:) :: (E.Assign s t old new, Show t, KnownSymbol s) => Var s -> ExprRepr repr t old -> repr old new
  print :: Show t => ExprRepr repr t old -> repr old old

infixl 1 >>
infix 2 =:

return :: ()
return  = ()
