module APL.Tests
  ( properties
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import APL.Parser (parseAPL)
import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , elements
  , suchThat
  , frequency
  , quickCheck
  , sample
  , vectorOf
  , listOf
  , chooseInt
  , choose
  )

instance Arbitrary Exp where
  arbitrary = sized (`genExp` [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

-- generateValidVName :: Gen VName
-- generateValidVName = do
--     alpha <- elements ['a' .. 'z']
--     alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
--     pure (alpha : alphaNums)

generateValidVName :: Gen VName
generateValidVName = do
    len <- choose (2, 4)  -- Randomly choose a length between 2 and 4
    alpha <- elements ['a' .. 'z']  -- First character is a letter
    alphaNums <- vectorOf (len - 1) (elements $ ['a' .. 'z'] ++ ['0' .. '9'])  -- Generate the rest of the characters
    pure (alpha : alphaNums)


-- The main expression generator
genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp size vars = frequency
  [ (20, CstInt <$> arbitrary)  -- 20% constant integers
  , (20, CstBool <$> arbitrary) -- 20% constant booleans
  , (9, Var <$> chooseVar vars) -- 9% variables, to ensure known variable coverage
  , (10, Add <$> genExp halfSize vars <*> genExp halfSize vars)  -- 10% addition
  , (10, Sub <$> genExp halfSize vars <*> genExp halfSize vars)  -- 10% subtraction
  , (5, Mul <$> genExp halfSize vars <*> genExp halfSize vars)  -- 5% multiplication
  , (15, Div <$> genExp halfSize vars <*> genExp halfSize vars)  -- 15% division (potential domain error)
  , (15, Pow <$> genExp halfSize vars <*> genExp halfSize vars)  -- 15% power (potential domain error)
  , (10, Eql <$> genExp halfSize vars <*> genExp halfSize vars)  -- 10% equality
  , (10, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars) -- 10% if expressions
  , (30, do
      newVar <- generateValidVName  -- Generate a unique valid variable name
      Let newVar <$> genExp halfSize (newVar : vars) <*> genExp halfSize (newVar : vars)) -- 30% let with a new variable
  , (20, do
      newVar <- generateValidVName  -- Generate a unique valid variable name
      Lambda newVar <$> genExp (size - 1) (newVar : vars)) -- 20% lambda with a new variable
  , (5, Apply <$> genExp halfSize vars <*> genExp halfSize vars) -- 5% function application
  , (5, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars) -- 5% try-catch
  ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

-- chooseVar :: [VName] -> Gen VName
-- chooseVar [] = generateValidVName -- Generate a new valid variable name if none exist
-- chooseVar vars = elements vars `suchThat` \v -> length v >= 2 && length v <= 4

chooseVar :: [VName] -> Gen VName
chooseVar [] = generateValidVName  -- Generate a new valid variable name if the list is empty
chooseVar vars = do
  let filteredVars = filter (\v -> length v >= 2 && length v <= 4) vars
  if null filteredVars
    then elements vars   -- Fall back to any variable if none match the length condition
    else elements filteredVars


generateUniqueVar :: [VName] -> Gen VName
generateUniqueVar existingVars =
  arbitrary `suchThat` isUnique
  where
    isUnique v = length v >= 2 && length v <= 4 && v `notElem` existingVars

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp defaultVars e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp defaultVars e)) "no domain error"
  . cover 20 (any isTypeError (checkExp defaultVars e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp defaultVars e)) "no type error"
  . cover 5 (any isVariableError (checkExp defaultVars e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp defaultVars e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()
  where
    defaultVars = ["x", "y", "z", "var1", "var2"]  -- Populate with meaningful variable names

parsePrinted :: Exp -> Bool
parsePrinted exp =
  case parseAPL "test" (printExp exp) of
    Left _ -> False
    Right parsedExp -> parsedExp == exp

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
