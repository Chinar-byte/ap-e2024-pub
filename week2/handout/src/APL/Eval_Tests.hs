module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

tests :: TestTree
tests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt 7)),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= ([], Left "Non-integer operand"),
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt (-3))),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= ([], Right (ValInt 8)),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= ([], Left "Negative exponent"),
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= ([], Right (ValBool False)),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= ([], Right (ValBool True)),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= ([], Right (ValInt 16)),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= ([], Right (ValBool True))
    ,
    testGroup
    "Print Evaluation"
    [ testCase "Print Integer" $
        eval' (Print "Result" (CstInt 42))
          @?= (["Result: 42"], Right (ValInt 42)),
      --
      testCase "Print Boolean True" $
        eval' (Print "Boolean" (CstBool True))
          @?= (["Boolean: True"], Right (ValBool True)),
      --
      testCase "Print Boolean False" $
        eval' (Print "Boolean" (CstBool False))
          @?= (["Boolean: False"], Right (ValBool False)),
      --
      testCase "Print Function" $
        eval' (Print "Function" (Lambda "x" (Mul (Var "x") (Var "x"))))
          @?= (["Function: #<fun>"], Right (ValFun [] "x" (Mul (Var "x") (Var "x")))),
      --
      testCase "Print in Let expression" $
        eval'
          (Let "x" (CstInt 10) (Print "x" (Var "x")))
          @?= (["x: 10"], Right (ValInt 10)),
      --
      testCase "Print after operation" $
        eval' (Print "Sum" (Add (CstInt 5) (CstInt 3)))
          @?= (["Sum: 8"], Right (ValInt 8)),
      --
      testCase "Print nested with If" $
        eval' (Print "If result" (If (CstBool True) (CstInt 1) (CstInt 0)))
          @?= (["If result: 1"], Right (ValInt 1)),
      --
      testCase "Print with division by zero" $
        eval' (Print "Division" (Div (CstInt 10) (CstInt 0)))
          @?= ([], Left "Division by zero")
       testCase "KvPut KvGet 1" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)))
          @?= ([], Right (ValBool True)),
      --
      testCase "KvPut KvGet 2" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1)))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "KvPut KvGet 3" $
        eval'
          ( Let
              "x"
              (KvPut (CstInt 0) (CstBool True))
              ( Let
                  "y"
                  (KvPut (CstInt 0) (CstBool False))
                  (KvGet (CstInt 0))
              )
          )
          @?= ([], Right (ValBool False))
    ]
    ]
