module DSL.AST where

import GHC.TypeLits (KnownSymbol)

import DSL.Env (VarTypeMap)
import DSL.Syntax (Expr (..), Syn (..), Var (..))

import qualified DSL.Env as E


data Statement (old :: VarTypeMap) (new :: VarTypeMap) where
  Seq :: Statement old map -> Statement map new -> Statement old new
  Assign :: (E.Assign s t old new, Show t, KnownSymbol s) => Var s -> Expr t old -> Statement old new
  Print :: Show t => Expr t old -> Statement old old

deriving instance Show (Statement old new)

instance Syn Statement where
  (>>) = Seq
  (=:) = Assign
  print = Print

showProgram :: Statement old new -> String
showProgram = show
