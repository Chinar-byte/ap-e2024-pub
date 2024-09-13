{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module APL.AST
  (
    Exp(..),
    VName
  )
where
    data Exp
      = CstInt Integer
      | Add Exp Exp
      | Sub Exp Exp
      | Mul Exp Exp
      | Div Exp Exp
      | Pow Exp Exp
      | CstBool Bool
      | Eql Exp Exp
      | If Exp Exp Exp
      | Var VName
      | Let VName Exp Exp
      | Lambda VName Exp 
      | Apply Exp Exp 
      | TryCatch Exp Exp
      deriving (Eq, Show)

    type VName = String