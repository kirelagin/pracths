module DSL.Syntax
  ( Var (..)
  , Expr (..)
  , Statement (..)

  , (>>)
  , (=:)
  , print
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


data Statement (old :: VarTypeMap) (new :: VarTypeMap) where
  Seq :: Statement old map -> Statement map new -> Statement old new
  Assign :: (E.Assign s t old new, Show t, KnownSymbol s) => Var s -> Expr t old -> Statement old new
  Print :: Show t => Expr t old -> Statement old old

deriving instance Show (Statement old new)

(>>) :: Statement old map -> Statement map new -> Statement old new
(>>) = Seq

infixl 1 >>

(=:) :: (E.Assign s t old new, Show t, KnownSymbol s) => Var s -> Expr t old -> Statement old new
(=:) = Assign

infix 2 =:

print :: Show t => Expr t old -> Statement old old
print = Print

return :: ()
return  = ()
