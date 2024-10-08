module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], Right x)
    runEval' r s (Free (ReadOp k)) = runEval' r s (k r)
    runEval' r s (Free (StateGetOp a)) = runEval' r s (a s)
    runEval' r _ (Free (StatePutOp s' b)) = runEval' r s' b
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' r s (Free (ErrorOp err)) = ([], Left err)
