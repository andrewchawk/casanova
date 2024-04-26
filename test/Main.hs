module Main (main) where

import Casanova
import System.Exit
import Data.Maybe
import Data.Either
import Data.Ratio

-- | If @x@ is recursively evaluated to @Right y@, then
-- @checkExpressionEquality s x (Just y)@ is 'Nothing'.  If @x@ is recursively
-- evaluated to some 'Left' value, then @checkExpressionEquality s x Nothing@
-- is 'Nothing'.  Otherwise, @checkExpressionEquality s x y@ is @'Just' s@.
checkExpEquality :: String -> Expression -> Maybe Expression -> Maybe String
checkExpEquality s x (Just y) = if e == Right y then Nothing else Just s2
  where
  e = recursiveExceptionallyEvaluate x
  s2 = s ++ " (" ++ expected ++ "; " ++ actual ++ ")"
    where
    expected = "Expected result: " ++ show (Right y :: Exceptional Expression)
    actual = "Actual result: " ++ show e
checkExpEquality s x Nothing = if isLeft e then Nothing else Just s2
  where
  e = recursiveExceptionallyEvaluate x
  s2 = s ++ " (Expecting a Left value but got " ++ show e ++ ")"

-- | If all values in @x@ are 'Nothing', then @sequenceEqualityErrors x@ is also
-- 'Nothing'.  Otherwise, @sequenceEqualityErrors x@ is a list of all 'Just'
-- elements of @x@.
sequenceEqualityErrors :: [Maybe String] -> Maybe [String]
sequenceEqualityErrors x = case catMaybes x of
  [] -> Nothing
  x@(_ : _) -> Just x

main :: IO ()
main = maybe exitSuccess (\t -> printFailMsg t >> exitFailure) equalChkResults
  where
  printFailMsg t = putStrLn "The following test cases failed:\n" >> mapM_ printError t
    where printError = putStrLn . ("- " ++)
  chkEquality (a,b,c) = checkExpEquality a b c
  equalChkResults = sequenceEqualityErrors $ map chkEquality $
    testsForDiff ++
    testsForFlip ++
    testsForLimit ++
    testsForLambda ++
    testsForExpRatio ++
    testsForExponents ++
     [("Dividing by zero zero",
      (\z -> Ap2Quotient z z) $ ExpRatio 0,
      Nothing),
     ("Square of square root of -1",
      let i = Ap2 Exponent (ExpRatio (-1)) $ ExpRatio $ 1 % 2 in
      Ap2 Exponent i $ ExpRatio 2,
      Just $ ExpRatio (-1)),
     ("Derivative of e^x",
      Ap1 (Diff "x") $ Ap2 Exponent Euler $ Variable "x",
      Just $ Ap2 Exponent Euler $ Variable "x"),
     ("Adding infinity and negative infinity",
      Ap2 Sum Infinity $ Ap1 Negate Infinity,
      Just $ ExpRatio 0),
     ("log(a^b,c)",
      Ap2 Logarithm (Ap2 Exponent (Variable "a") (Variable "b")) (Variable "c"),
      Just $ Ap2 Product (Variable "b") $
        Ap2 Logarithm (Variable "a") (Variable "c")),
     ("g ^ log(e,g)",
      Ap2 Exponent (Variable "g") $ Ap2 Logarithm (Variable "e") (Variable "g"),
      Just $ Variable "e")]

-- | Values of this type can be used as input for 'checkExpEquality' and,
-- therefore, are test cases.
type TestCase = (String, Expression, Maybe Expression)

-- | These tests pertain largely to 'Flip'.
testsForFlip :: [TestCase]
testsForFlip =
  [("Flipped addition with ExpRatio",
    Ap2 (Flip Sum) (ExpRatio 5) (ExpRatio 10),
    Just $ ExpRatio 15),
   ("Flipped multiplication with ExpRatio",
    Ap2 (Flip Product) (ExpRatio 5) (ExpRatio 10),
    Just $ ExpRatio 50)]

-- | This value is a list of test cases which mostly pertain to 'Diff'.
testsForDiff :: [TestCase]
testsForDiff =
  [("Derivative of the deriviative of sin x",
    Ap1 (Diff "x") $ Ap1 (Diff "x") $ Ap1 Sin $ Variable "x",
    Just $ Ap1 Negate $ Ap1 Sin $ Variable "x"),
   ("Derivative of x * x",
    Ap1 (Diff "x") $ Ap2 Product (Variable "x") $ Variable "x",
    Just $ Ap2 Product (ExpRatio 2) $ Variable "x"),
   ("Derivative of x^1",
    Ap1 (Diff "x") $ Ap2 Exponent (Variable "x") $ ExpRatio 1,
    Just $ ExpRatio 1),
   ("Derivative of the sine of an unrelated variable",
    Ap1 (Diff "z") $ Ap1 Sin $ Variable "x",
    Just $ Ap1 (Diff "z") $ Ap1 Sin $ Variable "x")]

-- | This value is a list of test cases which mostly pertain to 'Limit'.
testsForLimit :: [TestCase]
testsForLimit =
  [("Limit of infinity as x approaches infinity",
    Ap1 (Limit "x" Infinity) Infinity,
    Just Infinity),
   ("Limit of 5 as x aproaches infinity",
    Ap1 (Limit "x" Infinity) $ ExpRatio 5,
    Just $ ExpRatio 5),
   ("Limit of x as x approaches n",
    Ap1 (Limit "x" $ Variable "n") (Variable "x"),
    Just $ Variable "n"),
   ("Using a limit to calculate e",
    Ap1 (Limit "x" Infinity) $
      Ap2 (Flip Exponent)
        (Variable "x")
        (ExpRatio 1 + Ap2Quotient (ExpRatio 1) (Variable "x")),
    Just Euler),
   ("Limit of (a^2 + a) / a as a approaches 0",
    Ap1 (Limit "a" $ ExpRatio 0) $
      Ap2Quotient
        (Ap2 Sum (Ap2 Exponent (Variable "a") $ ExpRatio 2) (Variable "a"))
        (Variable "a"),
    Just $ ExpRatio 1),
   ("Limit of 1/x as x approaches infinity",
    Ap1 (Limit "x" Infinity) $ Ap2Quotient (ExpRatio 1) $ Variable "x",
    Just $ ExpRatio 0),
   ("Limit of 1/x as x approaches zero",
    Ap1 (Limit "x" $ ExpRatio 0) $ Ap2Quotient (ExpRatio 1) $ Variable "x",
    Just Infinity),
   ("Limit of x/x as x approaches zero",
    Ap1 (Limit "x" $ ExpRatio 0) $ (\x -> Ap2Quotient x x) $ Variable "x",
    Just $ ExpRatio 1),
   ("Limit of x / (x+1)",
    Ap1 (Limit "x" Infinity) $
      Ap2Quotient (Variable "x")
        (Ap2 Sum (Variable "x") (ExpRatio 1)),
    Just $ ExpRatio 1)]

-- | This value is a list of test cases which mostly pertain to 'Lambda'.
testsForLambda :: [TestCase]
testsForLambda =
  [("Simple lambda with unused variable",
    Ap1 (Lambda "x" Infinity) $ Ap1 Negate Infinity,
    Just Infinity),
   ("Potentially tricky lambda substitution",
    Ap1
      (Lambda "x" $
        Ap1 (Lambda "z" $ Variable "g") Infinity) $
      Ap1 Negate Infinity,
    Just $ Variable "g")]

-- | This value is a list of test cases which mostly pertain to 'ExpRatio'.
testsForExpRatio :: [TestCase]
testsForExpRatio =
  [("Basic ExpRatio addition",
    Ap2 Sum (ExpRatio a) (ExpRatio b),
    Just $ ExpRatio $ a + b),
   ("Basic ExpRatio subtraction",
    Ap2 Sum (ExpRatio a) (Ap1 Negate $ ExpRatio b),
    Just $ ExpRatio $ a - b),
   ("Basic ExpRatio multiplication",
    Ap2 Product (ExpRatio a) (ExpRatio b),
    Just $ ExpRatio $ a * b),
   ("Basic ExpRatio division",
    Ap2Quotient (ExpRatio a) (ExpRatio b),
    Just $ ExpRatio $ a / b),
   ("Exponent of ExpRatios with common denominator of one",
    Ap2 Exponent (ExpRatio 2) $ ExpRatio 5,
    Just $ ExpRatio $ 2 ^ 5),
   ("Negative exponent of ExpRatios with common denominator of one",
    Ap2 Exponent (ExpRatio 2) $ ExpRatio (- 5),
    Just $ ExpRatio $ 1 % (2 ^ 5))]
  where (a,b) = (2407620 % 45672, 28467 % 329057)

-- | These tests mostly pertain to exponentiation.
testsForExponents :: [TestCase]
testsForExponents =
  [("n^1",
    Ap2 Product (Variable "n") $ ExpRatio 1,
    Just $ Variable "n"),
   (let exp = Ap2 Exponent (Variable "n") $ Variable "n" in
    ("n^n", exp, Just exp)),
   ("0^0",
    Ap2 Exponent (ExpRatio 0) $ ExpRatio 0,
    Nothing),
   ("5^(-2)",
    Ap2 Exponent (ExpRatio 5) $ ExpRatio (-2),
    Just $ ExpRatio $ 1 % (5 ^ 2))]
