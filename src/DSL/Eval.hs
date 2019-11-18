module DSL.Eval
  ( evalExpr

  , runProgram
  ) where

import Prelude hiding (lookup)
import qualified Prelude as P

import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)

import DSL.Env (Assign (assign), Env, Lookup (lookup), empty)
import DSL.Syntax (Expr (..), Syn (..), Var (..))


evalExpr :: Expr t map -> Env map -> t
evalExpr (Lit v) _ = v
evalExpr (Lookup (Var :: Var s)) env = lookup @s env
evalExpr (Add e1 e2) env = evalExpr e1 env + evalExpr e2 env
evalExpr (Sub e1 e2) env = evalExpr e1 env - evalExpr e2 env
evalExpr (Mul e1 e2) env = evalExpr e1 env * evalExpr e2 env
evalExpr (Abs e) env = abs $ evalExpr e env
evalExpr (Sgn e) env = signum $ evalExpr e env
evalExpr (Len e) env = fromIntegral $ length (evalExpr e env)


newtype Eval m old new = Eval { evalStatement :: Env old -> m (Env new) }

instance MonadWriter [String] m => Syn (Eval m) where
  Eval s1 >> Eval s2 = Eval $ \env -> s1 env P.>>= s2
  (Var :: Var s) =: e = Eval $ \env -> pure $ assign @s (evalExpr e env) env
  print e = Eval $ \env -> tell [show $ evalExpr e env] P.>> pure env


runProgram :: Eval (Writer [String]) '[] new -> [String]
runProgram p = execWriter (evalStatement p empty)
