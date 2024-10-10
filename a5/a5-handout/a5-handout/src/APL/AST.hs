module APL.AST
  ( VName
  , Exp (..)
  , printExp
  , subExp
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

parens :: String -> String
parens x = "(" ++ x ++ ")"

printBinOp :: String -> Exp -> Exp -> String
printBinOp op x y = parens $ printExp x ++ " " ++ op ++ " " ++ printExp y

printExp :: Exp -> String
printExp (CstInt x) = show x
printExp (CstBool b) = if b then "true" else "false"
printExp (Add x y) = printBinOp "+" x y
printExp (Sub x y) = printBinOp "-" x y
printExp (Mul x y) = printBinOp "*" x y
printExp (Div x y) = printBinOp "/" x y
printExp (Pow x y) = printBinOp "**" x y
printExp (Eql x y) = printBinOp "==" x y
printExp (If x y z) =
  parens $
    "if "
      ++ printExp x
      ++ " then "
      ++ printExp y
      ++ " else "
      ++ printExp z
printExp (Var v) = v
printExp (Let v e1 e2) =
  parens $
    "let "
      ++ v
      ++ " = "
      ++ printExp e1
      ++ " in "
      ++ printExp e2
printExp (Lambda v body) =
  parens $ "\\" ++ v ++ " -> " ++ printExp body
-- printExp (Apply f (Apply f' x)) = 
--   printExp f ++ " (" ++ printExp f' ++ printExp x ++ ")"
printExp (Apply x y) =
  printExp x ++ " " ++ printExp y
printExp (TryCatch x y) =
  "try " ++ printExp x ++ " catch " ++ printExp y

subExp :: Exp -> [Exp]
subExp e = e : case e of
  CstInt _ -> []
  CstBool _ -> []
  Add e1 e2 -> subExp e1 ++ subExp e2
  Sub e1 e2 -> subExp e1 ++ subExp e2
  Mul e1 e2 -> subExp e1 ++ subExp e2
  Div e1 e2 -> subExp e1 ++ subExp e2
  Pow e1 e2 -> subExp e1 ++ subExp e2
  Eql e1 e2 -> subExp e1 ++ subExp e2
  If e0 e1 e2 -> subExp e0 ++ subExp e1 ++ subExp e2
  Var _ -> []
  Let _ e1 e2 -> subExp e1 ++ subExp e2
  Lambda _ body -> subExp body
  Apply e1 e2 -> subExp e1 ++ subExp e2
  TryCatch e1 e2 -> subExp e1 ++ subExp e2


printExp1 :: Exp -> String
printExp1 (CstInt n) = show n
printExp1 (CstBool b) = show b
printExp1 (Var x) = x
printExp1 (Add x y) = "(" ++ printExp1 x ++ " + " ++ printExp1 y ++ ")"
printExp1 (Sub x y) = "(" ++ printExp1 x ++ " - " ++ printExp1 y ++ ")"
printExp1 (Mul x y) = "(" ++ printExp1 x ++ " * " ++ printExp1 y ++ ")"
printExp1 (Div x y) = "(" ++ printExp1 x ++ " / " ++ printExp1 y ++ ")"
printExp1 (Pow x y) = "(" ++ printExp1 x ++ " ** " ++ printExp1 y ++ ")"
printExp1 (Eql x y) = "(" ++ printExp1 x ++ " == " ++ printExp1 y ++ ")"
printExp1 (If x y z) = "(if " ++ printExp1 x ++ " then " ++ printExp1 y ++ " else " ++ printExp1 z ++ ")"
printExp1 (Let x y z) = "(let " ++ x ++ " = " ++ printExp1 y ++ " in " ++ printExp1 z ++ ")"
printExp1 (Lambda name body) = "(\\" ++ name ++ " -> " ++ printExp1 body ++ ")"
printExp1 (Apply f (Apply f' x)) = printExp1 f ++ " (" ++ printExp1 f' ++ printExp1 x ++ ")"
printExp1 (Apply f x) = printExp1 f ++ " " ++ printExp1 x
printExp1 (TryCatch body handler) = "(try " ++ printExp1 body ++ " catch " ++ printExp1 handler ++ ")"