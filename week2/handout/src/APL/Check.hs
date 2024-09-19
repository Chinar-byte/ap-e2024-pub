{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module APL.Check
  ( checkExp,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

-- newtype CheckM a = CheckM ([VName] -> (Maybe Error, a))

-- instance Functor CheckM where
--   fmap f (CheckM m) = CheckM $ \env ->
--     let (err, x) = m env
--     in (err, f x)

-- instance Applicative CheckM where
--   pure x = CheckM $ \env -> (Nothing, x)
--   (<*>) = ap

-- instance Monad CheckM where
--   CheckM m >>= f = CheckM $ \env ->
--     let (err, x) = m env
--     in case err of
--          Just e  -> (Just e, undefined) -- The second part is not used
--          Nothing -> let (err', x') = runCheckM (f x) env
--                     in (err', x')

-- -- Helper function to check if a variable is in scope
-- checkVarInScope :: VName -> CheckM ()
-- checkVarInScope var = CheckM $ \env ->
--   if var `elem` env
--     then (Nothing, ())
--     else (Just $ "Variable not in scope: " ++ var, ())

-- -- Helper function to extend scope
-- extendScope :: VName -> CheckM a -> CheckM a
-- extendScope var (CheckM m) = CheckM $ \env -> m (var : env)

-- -- Run the CheckM monad
-- runCheckM :: CheckM a -> [VName] -> (Maybe Error, a)
-- runCheckM (CheckM m) = m

-- checkExp :: Exp -> Maybe Error
-- checkExp exp = case runCheckM (check exp) [] of
--   (Just err, _) -> Just err
--   (Nothing, _)  -> Nothing

-- check :: Exp -> CheckM ()
-- check (CstInt _) = return ()
-- check (CstBool _) = return ()
-- check (Var v) = checkVarInScope v
-- check (Add e1 e2) = check e1 >> check e2
-- check (Sub e1 e2) = check e1 >> check e2
-- check (Mul e1 e2) = check e1 >> check e2
-- check (Div e1 e2) = check e1 >> check e2
-- check (Pow e1 e2) = check e1 >> check e2
-- check (Eql e1 e2) = check e1 >> check e2
-- check (If cond e1 e2) = check cond >> check e1 >> check e2
-- check (Let var e1 e2) = do
--   check e1
--   extendScope var $ check e2
-- check (Lambda var body) = extendScope var $ check body
-- check (Apply e1 e2) = check e1 >> check e2
-- check (TryCatch e1 e2) = check e1 >> check e2
-- check (Print _ e1) = check e1
-- check (KvPut e1 e2) = check e1 >> check e2
-- check (KvGet e1) = check e1
