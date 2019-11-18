module DSL.AST
  ( Expr
  , Statement

  , showProgram
  ) where

import Data.Kind (Type)
import DSL.Env (VarTypeMap)
import DSL.Syntax (ExprSyn (..), Syn (..), Var (..))
import GHC.TypeLits (KnownSymbol)

import qualified DSL.Env as E


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

instance ExprSyn Expr where
  lit = Lit
  lkp = Lookup

  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  sgn = Sgn

  len = Len


data Statement (old :: VarTypeMap) (new :: VarTypeMap) where
  Seq :: Statement old map -> Statement map new -> Statement old new
  Assign :: (E.Assign s t old new, Show t, KnownSymbol s) => Var s -> Expr t old -> Statement old new
  Print :: Show t => Expr t old -> Statement old old

deriving instance Show (Statement old new)

instance Syn Statement where
  type ExprRepr Statement = Expr
  (>>) = Seq
  (=:) = Assign
  print = Print

showProgram :: Statement old new -> String
showProgram = show
