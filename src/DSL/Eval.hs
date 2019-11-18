module DSL.Eval
  ( evalExpr

  , runProgram
  ) where

import Prelude hiding (lookup)
import qualified Prelude as P

import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)

import DSL.Env (Assign (assign), Env, Lookup (lookup), empty)
import DSL.Syntax (ExprSyn (..), Syn (..), Var (..))


newtype ExprEval t map = ExprEval { evalExpr :: Env map -> t }

instance ExprSyn ExprEval where
  lit v = ExprEval $ \_ -> v
  lkp (Var :: Var s) = ExprEval $ \env -> lookup @s env

  ExprEval e1 + ExprEval e2 = ExprEval $ \env -> e1 env P.+ e2 env
  ExprEval e1 - ExprEval e2 = ExprEval $ \env -> e1 env P.- e2 env
  ExprEval e1 * ExprEval e2 = ExprEval $ \env -> e1 env P.* e2 env
  abs (ExprEval e) = ExprEval $ \env -> P.abs (e env)
  sgn (ExprEval e) = ExprEval $ \env -> P.signum (e env)

  len (ExprEval e) = ExprEval $ \env -> fromIntegral $ length (e env)


newtype Eval m old new = Eval { evalStatement :: Env old -> m (Env new) }

instance MonadWriter [String] m => Syn (Eval m) where
  type ExprRepr (Eval m) = ExprEval
  Eval s1 >> Eval s2 = Eval $ \env -> s1 env P.>>= s2
  (Var :: Var s) =: e = Eval $ \env -> pure $ assign @s (evalExpr e env) env
  print e = Eval $ \env -> tell [show $ evalExpr e env] P.>> pure env


runProgram :: Eval (Writer [String]) '[] new -> [String]
runProgram p = execWriter (evalStatement p empty)
