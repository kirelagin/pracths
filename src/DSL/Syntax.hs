module DSL.Syntax
  ( Var (..)
  , Expr (..)
  , Syn (..)

  , return

  , module R
  ) where

import Prelude hiding (return, (>>), print)

import Prelude as R (($), Num (..))

import Data.Kind (Type)
import Data.String as R (IsString (fromString))
import GHC.OverloadedLabels as R (IsLabel (fromLabel))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import DSL.Env (VarTypeMap)

import qualified DSL.Env as E


data Var (s :: Symbol) = Var

instance KnownSymbol s => Show (Var s) where
  show Var = symbolVal (Var :: Var s)

instance s1 ~ s => IsLabel s1 (Var s) where
  fromLabel = Var @s


data Expr (t :: Type) (map :: VarTypeMap) where
  Lit :: t -> Expr t map
  Lookup :: (E.Lookup s t map, KnownSymbol s) => Var s -> Expr t map

  Add :: Expr Integer map -> Expr Integer map -> Expr Integer map
  Sub :: Expr Integer map -> Expr Integer map -> Expr Integer map
  Mul :: Expr Integer map -> Expr Integer map -> Expr Integer map
  Abs :: Expr Integer map -> Expr Integer map
  Sgn :: Expr Integer map -> Expr Integer map

  Len :: Expr String map -> Expr Integer map

deriving instance Show t => Show (Expr t map)

instance t ~ String => IsString (Expr t map) where
  fromString = Lit

instance (E.Lookup s t map, KnownSymbol s) => IsLabel s (Expr t map) where
  fromLabel = Lookup $ Var @s


--instance Num (Expr Integer) where
instance t ~ Integer => Num (Expr t map) where
  fromInteger = Lit
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  signum = Sgn


class Syn repr where
  (>>) :: repr old map -> repr map new -> repr old new
  (=:) :: (E.Assign s t old new, Show t, KnownSymbol s) => Var s -> Expr t old -> repr old new
  print :: Show t => Expr t old -> repr old old

infixl 1 >>
infix 2 =:

return :: ()
return  = ()
