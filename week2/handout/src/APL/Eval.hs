{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module APL.Eval
  ( Val (..),
    Env,
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import Control.Monad.Trans.Accum (evalAccum)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

storeEmpty :: Store
storeEmpty = []

askStore :: EvalM Store
askStore = EvalM $ \env store -> ([], Right store)

storeLookup :: Val -> Store -> Maybe Val
storeLookup v store = lookup v store

storeExtend :: Key -> Val -> Store -> Store
storeExtend k val s = (k, val) : s

localStore :: (Store -> Store) -> EvalM a -> EvalM a
localStore f (EvalM m) = EvalM $ \env store -> m env (f store)

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

evalKvGet :: Key -> EvalM Val
evalKvGet key = do
  store <- askStore
  case storeLookup key store of
    Just val -> EvalM $ \env store -> ([show store ++ "getting"], Right val)
    Nothing  -> failure ("Invalid Key: " ++ showVal key ++ show store)

showVal :: Val -> String
showVal (ValInt x)     = "ValInt " ++ show x
showVal (ValBool x)    = "ValBool " ++ show x
showVal (ValFun _ x _) = "ValFun " ++ show x

evalKvPut :: Key -> Val -> EvalM ()
evalKvPut key val = do
  a <- localStore (storeExtend key val) $ EvalM $ \env store -> ([], Right ())
  store <- askStore
  EvalM $ \env store -> (["putting" ++ show store], Right ())

type Error = String
type Key = Val

type Store = [(Key, Val)]

newtype EvalM a = EvalM (Env -> Store -> ([String], Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \env store -> ([], Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env store ->
    let (logs1, resX) = x env store
    in case resX of
         Left err -> (logs1, Left err)
         Right x' ->
           let EvalM y = f x'
               (logs2, resY) = y env store
           in (logs1 ++ logs2, resY)


askEnv :: EvalM Env
askEnv = EvalM $ \env store -> ([], Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \env store -> ([], Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env store ->
  case m1 env store of
    (logs, Left _) -> m2 env store
    (logs, Right x) -> (logs, Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = m envEmpty storeEmpty

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print str e1) = do
  v1 <- eval e1
  case v1 of
    ValInt x -> do
      printEval (str ++ ": " ++ show x)
      return (ValInt x)
    ValBool x -> do
      printEval (str ++ ": " ++ show x)
      return (ValBool x)
    ValFun env x e1 -> do
      printEval (str ++ ": #<fun>")
      return (ValFun env x e1)
eval (KvPut e1 e2) = do 
  key <- eval e1
  val <- eval e2
  -- localStore (storeExtend key val) $ pure val
  evalKvPut key val
  return val
eval (KvGet e1) = do 
  key <- eval e1
  evalKvGet key


printEval :: String -> EvalM ()
printEval msg = EvalM $ \env store ->
  ([msg], Right ())

