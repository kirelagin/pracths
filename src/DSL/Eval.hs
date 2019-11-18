module DSL.Eval
  ( evalExpr
  , evalStatement

  , runProgram
  , showProgram
  ) where

import Prelude hiding (lookup)

import Control.Monad.Writer (MonadWriter (tell), execWriter)

import DSL.Env (Assign (assign), Env, Lookup (lookup), empty)
import DSL.Syntax (Expr (..), Statement (..), Var (..))


evalExpr :: Expr t map -> Env map -> t
evalExpr (Lit v) _ = v
evalExpr (Lookup (Var :: Var s)) env = lookup @s env
evalExpr (Add e1 e2) env = evalExpr e1 env + evalExpr e2 env
evalExpr (Sub e1 e2) env = evalExpr e1 env - evalExpr e2 env
evalExpr (Mul e1 e2) env = evalExpr e1 env * evalExpr e2 env
evalExpr (Abs e) env = abs $ evalExpr e env
evalExpr (Sgn e) env = signum $ evalExpr e env
evalExpr (Len e) env = fromIntegral $ length (evalExpr e env)


evalStatement :: MonadWriter [String] m => Statement old new -> Env old -> m (Env new)
evalStatement (Seq s1 s2) env = evalStatement s1 env >>= evalStatement s2
evalStatement (Assign (Var :: Var s) e) env = pure $ assign @s (evalExpr e env) env
evalStatement (Print e) env = tell [show $ evalExpr e env] >> pure env


runProgram :: Statement '[] new -> [String]
runProgram p = execWriter (evalStatement p empty)

showProgram :: Statement old new -> String
showProgram = show
