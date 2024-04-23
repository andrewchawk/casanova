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
checkExpEquality s x (Just y) = if match then Nothing else Just s
  where match = recursiveExceptionallyEvaluate x == Right y
checkExpEquality s x Nothing = if isLeft e then Nothing else Just s
  where e = recursiveExceptionallyEvaluate x

sequenceEqualityErrors :: [Maybe String] -> Maybe [String]
sequenceEqualityErrors x = case catMaybes x of
  [] -> Nothing
  x@(_ : _) -> Just x

main :: IO ()
main = maybe exitSuccess (\t -> printFailMsg t >> exitFailure) equalChkResults
  where
  printFailMsg t = putStrLn $ "The following test cases failed:\n" ++ failList
    where failList = unlines $ map ("- " ++) t
  chkEquality (a,b,c) = checkExpEquality a b c
  equalChkResults = sequenceEqualityErrors $ map chkEquality $
    [("Limit of infinity as x approaches infinity",
      Ap1 (Limit "x" Infinity) Infinity,
      Just Infinity),
     ("Limit of x as x approaches n",
      Ap1 (Limit "x" $ Variable "n") (Variable "x"),
      Just $ Variable "n"),
     ("Dividing by zero zero",
      (\z -> Ap2 Quotient z z) $ ExpRatio $ 0 % 1,
      Nothing),
     ("Square of square root of -1",
      Ap2 (Flip Exponent) (ExpRatio $ 2 % 1) $ Ap2 Exponent (ExpRatio $ (-1) % 1) $ ExpRatio $ 1 % 2,
      Just $ ExpRatio $ (-1) % 1),
     ("Simple lambda with unused variable",
      Ap1 (Lambda "x" Infinity) $ Ap1 Negate Infinity,
      Just Infinity),
     ("Potentially tricky lambda substitution",
      Ap1 (Lambda "x" $ Ap1 (Lambda "z" $ Variable "g") Infinity) $ Ap1 Negate Infinity,
      Just $ Variable "g"),
     ("Derivative of the deriviative of sin x",
      Ap1 (Diff "x") $ Ap1 (Diff "x") $ Ap1 Sin $ Variable "x",
      Just $ Ap1 Negate $ Ap1 Sin $ Variable "x"),
     ("Derivative of the sine of an unrelated variable",
      Ap1 (Diff "z") $ Ap1 Sin $ Variable "x",
      Just $ Ap1 (Diff "z") $ Ap1 Sin $ Variable "x"),
     ("Adding infinity and negative infinity",
      Ap2 Sum Infinity $ Ap1 Negate Infinity,
      Just $ ExpRatio $ 0 % 1),
     ("Normal form conversion of x * 2/1",
      Ap2 Product (Variable "x") (ExpRatio $ 2 % 1),
      Just $ Ap2 Product (ExpRatio $ 2 % 1) (Variable "x")),
     (let (a,b) = ((2407620, 45672), (28467, 329057)) in
      ("Basic ExpRatio addition",
       Ap2 Sum (ExpRatio $ fst a % snd a) (ExpRatio $ fst b % snd b),
       Just $ ExpRatio $ (fst a % snd a) + (fst b % snd b))),
     (let (a,b) = ((2407620, 45672), (28467, 329057)) in
      ("Basic ExpRatio subtraction",
       Ap2 Sum (ExpRatio $ fst a % snd a) (Ap1 Negate $ ExpRatio $ fst b % snd b),
       Just $ ExpRatio $ (fst a % snd a) - (fst b % snd b))),
     (let (a,b) = ((2407620, 45672), (28467, 329057)) in
      ("Basic ExpRatio multiplication",
       Ap2 Product (ExpRatio $ fst a % snd a) (ExpRatio $ fst b % snd b),
       Just $ ExpRatio $ (fst a % snd a) * (fst b % snd b))),
     (let (a,b) = ((2407620, 45672), (28467, 329057)) in
      ("Basic ExpRatio division",
       Ap2 Quotient (ExpRatio $ fst a % snd a) (ExpRatio $ fst b % snd b),
       Just $ ExpRatio $ (fst a % snd a) / (fst b % snd b)))]
