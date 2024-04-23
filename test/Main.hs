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
     ("Dividing by zero zero",
      (\z -> Ap2 Quotient z z) $ ExpRatio $ 0 % 1,
      Nothing),
     ("Simple lambda with unused variable",
      Ap1 (Lambda "x" Infinity) $ Ap1 Negate Infinity,
      Just Infinity),
     ("Potentially tricky lambda substitution",
      Ap1 (Lambda "x" $ Ap1 (Lambda "z" $ Variable "g") Infinity) $ Ap1 Negate Infinity,
      Just $ Variable "g"),
     ("Derivative of the deriviative of sin x",
      Ap1 (Diff "x") $ Ap1 (Diff "x") $ Ap1 Sin $ Variable "x",
      Just $ Ap1 Negate $ Ap1 Sin $ Variable "x"),
     ("Adding infinity and negative infinity",
      Ap2 Sum Infinity $ Ap1 Negate Infinity,
      Just $ ExpRatio $ 0 % 1),
     (let (a,b) = ((2407620, 45672), (28467, 329057)) in
      ("Basic ExpRatio addition",
       Ap2 Sum (ExpRatio $ fst a % snd a) (ExpRatio $ fst b % snd b),
       Just $ ExpRatio $ (fst a % snd a) + (fst b % snd b)))]
