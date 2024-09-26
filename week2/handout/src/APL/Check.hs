{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module APL.Check
  ( checkExp,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import GHC.IO.Handle.Types (checkHandleInvariants)

type Error = String

-- Monad type
newtype CheckM a = CheckM (Exp -> Maybe Error)

-- Functor
instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure _ = CheckM $ \_ -> Just "Variable not in scope: "
  (<*>) = ap

instance Monad CheckM where 
  return = pure 
  (>>=) (CheckM a) f = CheckM $ \exp ->
    case a exp of 
      Just x -> pure x
      Nothing -> 



checkExp :: Exp -> Maybe Error 
checkExp = undefined

