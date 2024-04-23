import Data.Ratio
import Data.Either
import Data.Complex

-- | This datatype facilitates error handling.  @Left@ values indicate -- and
-- contain useful descriptions of -- errors which are encountered when
-- computing.  @Right@ values contain legitimate results.
type Exceptional a = Either String a

-- | Each value of type 'Expression' is a real number /or/ positive or negative
-- infinity.
--
-- = The Lack of Support for Floating-Point Numbers
--
-- Casanova lacks obvious support for floating-point numbers because
-- floating-point arithmetic is imprecise, accumulates errors, and can mislead. 
-- Instead, Casanova uses arbitrary-precision ratios and a dedicated value for
-- infinity.  The properties of floating-point arithmetic enable the direct
-- translation of floating-point values, so nothing is really lost.
data Expression =
  -- | This bit is used to represent variables.  The 'String' value is the
  -- name of the variable.  Using empty variable names is allowed but is
  -- /not/ recommended.
  Variable String |
  -- This bit is used to represent fractions and integers.  After all, an
  -- integer is really just a fraction whose denominator is 1.
  ExpRatio (Ratio Integer) |
  -- | 'Infinity' is just infinity.  This value behaves like the infinity of the
  -- floating-point numbers.
  Infinity |
  -- | This value behaves like the negative infinity of the floating-point
  -- numbers.
  NegativeInfinity |
  -- | Any such value represents the application of a function to the specified
  -- expression.
  Ap1 FunctionM1 Expression |
  -- | Any such value represents the application of a function to the specified
  -- expressions.
  Ap2 FunctionM2 Expression Expression
  deriving (Show, Eq)

-- | Values of type 'FunctionM1' are convenient, pattern-matching-friendly
-- representations of functions of type @Expression -> Exceptional Expression@.
-- @Exceptional@ is used because some of these functions, such as 'Tan', are
-- undefined at some points.
data FunctionM1 =
  -- | Any such value is a lambda expression such that the bound variable name
  -- and body of this lambda expression are the 'String' value and the
  -- 'Expression' value, respectively.
  Lambda String Expression |
  -- | 'Limit' is the higher-order limit function.  @Ap1 (Limit x a) m@ is the
  -- limit of @m@ as the variable whose name is @x@ approaches @m@.
  Limit String Expression |
  -- | 'Diff' is the higher-order differential function.  @Ap1 (Diff x) n@ is
  -- the differential, with regard to the variable whose name is @x@, of @n@.
  Diff String |
  -- | 'Integral' is the higher-order integration function.
  -- @Ap1 (Integral x) n@ is the integral, with regard to the variable whose
  -- name is @x@, of @n@.
  Integral String |
  -- | This value corresponds to 'abs'.
  Abs |
  -- | This value corresponds to 'signum'.
  Signum |
  -- | This value represents the negation function.
  Negate |
  -- | This value represents the ceiling function.
  Ceiling |
  -- | This value represents the floor function.
  Floor |
  -- | This value represents the sine function.
  Sin |
  -- | This value represents the cosine function.
  Cos |
  -- | This value represents the tangent function.
  Tan |
  -- | This value represents the cosecant function.
  Csc |
  -- | This value represents the secant function.
  Sec |
  -- | This value represents the cotangent function.
  Cot
  deriving (Show, Eq)

-- | Values of type 'FunctionM2' are convenient, pattern-matching-friendly
-- representations of functions of type
-- @Expression -> Expression -> Exceptional Expression@.
-- @Exceptional@ is used because some of these functions, such as 'Minimum',
-- are undefined at some points.
data FunctionM2 =
  -- | This constructor is used to flip the order of the arguments.
  -- @Ap2 (Flip Sum) m n@ is equivalent to @Ap2 Sum n m@.
  Flip FunctionM2 |
  -- | This value represents the sum function.
  Sum |
  -- | This value represents the product function.
  Product |
  -- | This value represents the division function.  @Ap2 Quotient a b@ is the
  -- quotient of @a@ and @b@.
  Quotient |
  -- | This value represents the exponentation function.
  Exponent |
  -- | This value is the function which outputs the minimum of the input
  -- arguments.
  Minimum |
  -- | This value is the function which outputs the maximum of the input
  -- arguments.
  Maximum
  deriving (Show, Eq)

-- | @subst1 n x f@ is the result of replacing with @x@ all of @f@'s bound
-- instances of @Variable n@, skipping certain lambda expressions appropriately.
subst1 :: String
       -- ^ This argument is the name of the variable which should be replaced.
       -> Expression
       -- ^ This value should be substituted for the specified variable.
       -> Expression
       -- ^ The output is the result of substituting for the bound instances in
       -- /this expression/.
       -> Expression
subst1 x z f = case f of
  original@(Variable x') -> if x' == x then z else original
  -- Lambda expressions demand special handling; if the name of the bound
  -- variable is @x@, then instances of @x@ in the body of the lambda expression
  -- should NOT be replaced.  In any case, though, the substitution *does* apply
  -- to the input of the lambda expression.
  Ap1 original@(Lambda x' f') m -> Ap1 newLambda $ subst1 x z m
    where
    newLambda = if x' == x then original else Lambda x' $ subst1 x z f'
  Ap1 g m -> Ap1 g $ subst1 x z m
  Ap2 g m n -> Ap2 g (subst1 x z m) (subst1 x z n)
  m -> m

-- | @subst@ is basically a version of 'subst1' which supports the substitution
-- of multiple variables.
subst :: [(String, Expression)]
      -- ^ Any element of this list is a tuple whose first and second elements
      -- are the name of the variable for which a value should be substituted
      -- and the value which should substitute the variable, respectively.
      -> Expression
      -- ^ The output is the result of substitution on this expression.
      -> Expression
subst pairs e = foldr (uncurry subst1) e pairs

-- | @simplify@ is the single-step simplification function.  @simplify x@ is
-- equivalent to @x@ but can be slightly more easily computed.
-- The simplification is mostly meant for calculation, as opposed to reading.
-- For some values, @simplify@ may actually output a value which is /less/
-- readable.
--
-- If you need documentation, then you probably want to use 'recursiveSimplify'.
simplify :: Expression -> Expression
simplify o = case o of
  Variable _ -> o
  ExpRatio _ -> o
  Infinity -> o
  NegativeInfinity -> o
  Ap1 (Limit n x) m -> case m of
    Variable n2 -> if n2 == n then x else o
    Ap2 f a b -> Ap2 f (Ap1 (Limit n x) a) (Ap1 (Limit n x) b)
    _ -> Ap1 (Limit n $ simplify x) $ simplify m
  Ap1 (Lambda n x) m -> subst1 n m x
  Ap1 (Diff x) m -> case m of
    Infinity -> ExpRatio $ 0 % 1
    NegativeInfinity -> ExpRatio $ 0 % 1
    ExpRatio _ -> ExpRatio $ 0 % 1
    Variable y -> if y == x then ExpRatio (1 % 1) else o
    Ap1 Sin a
      | recursiveSimplify a == Variable x -> Ap1 Cos a
      | otherwise -> o
    Ap1 Cos a
      | recursiveSimplify a == Variable x -> Ap1 Negate $ Ap1 Sin a
      | otherwise -> o
    Ap1 Tan a
      | recursiveSimplify a == Variable x -> Ap2 Exponent (Ap1 Sec a) (ExpRatio $ 2 % 1)
      | otherwise -> o
    Ap1 Csc a
      | recursiveSimplify a == Variable x -> Ap2 Product (Ap1 Negate $ Ap1 Cot a) (Ap1 Csc a)
      | otherwise -> o
    Ap1 Sec a
      | recursiveSimplify a == Variable x -> Ap2 Product (Ap1 Sec a) (Ap1 Tan a)
      | otherwise -> o
    Ap1 Cot a
      | recursiveSimplify a == Variable x -> Ap1 Negate $ square $ Ap1 Csc a
      | otherwise -> o
      where square x = Ap2 Exponent x $ ExpRatio $ 2 % 1
    Ap2 Product a b -> Ap2 Sum (diff1 x a b) (diff1 x b a)
      where diff1 x2 a2 b2 = Ap2 Product (Ap1 (Diff x2) a2) b2
    _ -> Ap1 (Diff x) $ simplify m
  Ap1 f x -> Ap1 f $ simplify x
  Ap2 (Flip f) m n -> Ap2 f n m
  Ap2 Sum (ExpRatio a) (ExpRatio b) -> ExpRatio $ a + b
  Ap2 Sum Infinity NegativeInfinity -> ExpRatio $ 0 % 1
  Ap2 Sum a b
    | a == b -> Ap2 Product a $ ExpRatio $ 2 % 1
    | isZero a == Just True -> b
    | isZero b == Just True -> a
    | otherwise -> Ap2 Sum (simplify a) (simplify b)
  Ap2 Product (ExpRatio a) (ExpRatio b) -> ExpRatio $ a * b
  Ap2 Product a b
    | a == b -> Ap2 Exponent a $ ExpRatio $ 2 % 1
    | Just True `elem` map isZero [a,b] -> ExpRatio $ 0 % 1
    | isOne a == Just True -> b
    | isOne b == Just True -> a
    | otherwise -> Ap2 Product (simplify a) (simplify b)
  Ap2 Quotient (ExpRatio a) (ExpRatio b) -> ExpRatio $ a / b
  Ap2 Quotient a b -> Ap2 Quotient (simplify a) (simplify b)
  Ap2 Exponent (ExpRatio a) (ExpRatio b)
    | denominator a * denominator b == 1 -> ExpRatio $ (numerator a ^ numerator b) % 1
    | denominator b == 1 -> ExpRatio $ iterate2 (* a) a (numerator b - 1)
    | otherwise -> o
  Ap2 Exponent a b
    | all (== Just True) (map isZero [a,b]) -> Ap2 Exponent (simplify a) (simplify b)
    | isZero a == Just True -> ExpRatio $ 0 % 1
    | isZero b == Just True -> ExpRatio $ 1 % 1
    | isOne a == Just True -> ExpRatio $ 1 % 1
    | otherwise -> Ap2 Exponent (simplify a) (simplify b)
  Ap2 f m n -> Ap2 f (simplify m) (simplify n)

-- | Whereas @simplify@ performs a single step of simplification,
-- @recursiveSimplify@ recursively applies simplification until no further
-- simplification can be achieved.
recursiveSimplify :: Expression
                  -- ^ The simpler equivalent of this expression is output.
                  -> Expression
recursiveSimplify x = if simplify x == x then x else recursiveSimplify $ simplify x

-- | @iterate f x n@ is the result of applying to @x@ @f@ @min 0 n@ times.
iterate2 :: (a -> a)
         -- ^ Iteration is done with this function.
         -> a
         -- ^ The function is first applied to this value.
         -> Integer
         -- ^ This value is the number of iterations.
         -> a
iterate2 f x n = if n <= 0 then x else f $ iterate2 f x $ n - 1

-- | If the input expression is certainly zero, then the output is 'Just'
-- 'True'.  If the input expression is definitely /not/ zero, then the output
-- is 'Just' 'False'.  If no answer is certain, then the output is 'Nothing'.
isZero :: Expression -> Maybe Bool
isZero (Variable _) = Nothing
isZero Infinity = Just False
isZero NegativeInfinity = Just False
isZero (ExpRatio x) = Just $ numerator x == 0
isZero x = if simplify x == x then Nothing else isZero (simplify x)

-- | If the input is certainly one, then the output is 'Just' 'True'.
-- If the input is certainly /not/ one, then the output is 'Just' 'False'.
-- If no answer is certain, then the output is 'Nothing'.
isOne :: Expression -> Maybe Bool
isOne (Variable _) = Nothing
isOne Infinity = Just False
isOne NegativeInfinity = Just False
isOne (ExpRatio x) = Just $ x == 1 % 1
isOne x = if simplify x == x then Nothing else isZero (simplify x)

-- | If the input contains even a single 'Right' value, then the output is the
-- first such 'Right' value.  Otherwise, the output is basically just a
-- concatenation of the error descriptions.
exceptionalSequence :: [Exceptional Expression] -> Exceptional Expression
exceptionalSequence [] = Left "The input for exceptionalSequence is empty!"
exceptionalSequence x = case rights x of
  [] -> Left $ "The following errors were reported:\n" ++ unlines (lefts x)
  (x : _) -> Right x

-- | If the input expression is in some way malformed -- for example, contains
-- the quotient of some number and zero -- then the output is a description of
-- the badness.  Otherwise, the 'Right' 'Expresion' which is output is
-- equivalent to the input but may be simpler.
--
-- @exceptionallyEvaluate@ performs a single step of the simplification process.
--
-- Also, @exceptionallyEvaluate@ /will/ convert to a standard form some
-- expressions.  This conversion facilitates processes like replacing with the
-- equivalent of "2 * a" the equivalent of "a + a".  This /replacement/
-- facilitates some computer algebra operations which are more obviously
-- desirable, e.g., the simplification of symbolic expressions.
exceptionallyEvaluate :: Expression -> Exceptional Expression
exceptionallyEvaluate o = case o of
  Variable _ -> Right o
  Infinity -> Right o
  NegativeInfinity -> Right o
  ExpRatio _ -> Right o
  Ap1 (Lambda x n) m -> Right $ subst1 x m n
  Ap1 (Limit x n) m -> case m of
    Variable x2 -> Right $ if x == x2 then n else o
    Ap2 Quotient m1 m2 -> exceptionalSequence $ map recursiveExceptionallyEvaluate
      [Ap2 Quotient (Ap1 (Limit x n) m1) (Ap1 (Limit x n) m2),
       subst1 x n m]
    _
      | subst1 x n m == m -> Right m
      | otherwise -> Right o
    ExpRatio b -> Right m
  Ap1 (Diff x) m -> case m of
    Variable x2 -> Right $ if x2 == x then ExpRatio (1 % 1) else o
    Infinity -> Right $ ExpRatio $ 0 % 1
    NegativeInfinity -> Right $ ExpRatio $ 0 % 1
    ExpRatio _ -> Right $ ExpRatio $ 0 % 1
    Ap2 Product m1 m2 ->
      Right $ Ap2 Sum
        (Ap2 Product (Ap1 (Diff x) m1) m2)
        (Ap2 Product (Ap1 (Diff x) m2) m1)
    Ap1 Negate f -> Right $ Ap1 Negate $ Ap1 (Diff x) f
    Ap1 f (Ap1 g e) -> Ap2 Product gDiff <$> diffCompose
      where
      diff f x = Lambda x $ Ap1 (Diff x) $ Ap1 f $ Variable x
      gDiff = Ap1 (Diff x) $ Ap1 g e
      diffCompose = flip Ap1 (Ap1 g e) <$> diffF
      -- At least in the case of sin (sin x), the following use of
      -- exceptionallyEvaluate prevents infinite recursion.  Debugging this part
      -- burned up a decent chunk of my time, and unless the handling of lambdas
      -- is fundamentally changed, we should probably keep using
      -- exceptionallyEvaluate.
      diffF = Lambda x <$> exceptionallyEvaluate (Ap1 (Diff x) $ Ap1 f e)
    Ap1 Sin m2
      | m2 == Variable x -> Right $ Ap1 Cos m2
    Ap1 Cos m2
      | m2 == Variable x -> Right $ Ap1 Negate $ Ap1 Sin m2
    Ap1 Tan m2
      | m2 == Variable x -> Right $ Ap2 Exponent (Ap1 Sec m2) (ExpRatio $ 2 % 1)
    Ap1 Csc m2
      | m2 == Variable x -> Right $ Ap2 Product (Ap1 Negate $ Ap1 Cot m2) (Ap1 Csc m2)
    Ap1 Sec m2
      | m2 == Variable x -> Right $ Ap2 Product (Ap1 Sec m2) (Ap1 Tan m2)
    Ap1 Cot m2
      | m2 == Variable x -> Right $ Ap1 Negate $ square $ Ap1 Csc m2
      where square x = Ap2 Exponent x $ ExpRatio $ 2 % 1
  Ap1 f x -> Ap1 f <$> exceptionallyEvaluate x
  Ap2 Product (Ap1 Negate a) b -> Right $ Ap1 Negate $ Ap2 Product a b
  Ap2 Product (ExpRatio a) (ExpRatio b) -> Right $ ExpRatio $ a * b
  Ap2 Product a b
    | Infinity `elem` [a,b] -> Right o
  Ap2 Quotient a b
    | isZero b == Just True -> Left "Division by zero is undefined."
    | isOne b == Just True -> Right a
    | recursiveSimplify a == recursiveSimplify b -> Right $ ExpRatio $ 1 / 1
    | otherwise -> case (a, b) of
        (ExpRatio a, ExpRatio b) -> Right $ ExpRatio $ a / b
        _ -> Right o
  Ap2 Sum a b
      -- The evaluation is just useful for combining addition expressions into
      -- multiplication expressions.
    | a2 == b2 && isRight a2 -> Right $ Ap2 Product (ExpRatio $ 2 % 1) a
    where [a2,b2] = map recursiveExceptionallyEvaluate [a,b]
  -- These "Ap2 f" matches which refer to variables are really just useful
  -- for converting expressions into the normal form.  No real computation
  -- is happening here, so this part can be resonably skipped.  Basically,
  -- constants precede variables, but variables precede function application
  -- expressions.
  Ap2 f fa@(Ap1 _ _) v@(Variable _) -> Right $ if isCommutative f then Ap2 f v fa else o
  Ap2 f fa@(Ap2 _ _ _) v@(Variable _) -> Right $ if isCommutative f then Ap2 f v fa else o
  Ap2 f v@(Variable _) n@(ExpRatio _) -> Right $ if isCommutative f then Ap2 f n v else o
  Ap2 f v@(Variable _) n@Infinity -> Right $ if isCommutative f then Ap2 f n v else o
  Ap2 f v@(Variable _) n@NegativeInfinity -> Right $ if isCommutative f then Ap2 f n v else o
  Ap2 f a b -> Ap2 f <$> exceptionallyEvaluate a <*> exceptionallyEvaluate b

-- | @recursiveExceptionallyEvaluate@ is like 'exceptionallyEvaluate' but
-- performs multiple evaluation steps.  The output is contains a simplification
-- or a description of the first error which is encountered.
recursiveExceptionallyEvaluate :: Expression -> Exceptional Expression
recursiveExceptionallyEvaluate x = either Left recurse e
  where
  e = exceptionallyEvaluate x
  recurse x2 = if x2 == x then Right x else recursiveExceptionallyEvaluate x2

-- | @isCommutative x@ is 'True' if and only if @x@ represents a commutative
-- function.
isCommutative :: FunctionM2 -> Bool
isCommutative Sum = True
isCommutative Product = True
isCommutative _ = False

-- | The standard Haskell numeric operators can be used instead of Casanova's
-- ever-verbose internal representations.
instance Num Expression where
  (+) a b = Ap2 Sum a b
  (-) a b = Ap2 Sum a $ Ap1 Negate b
  (*) a b = Ap2 Product a b
  fromInteger x = ExpRatio $ x % 1
  abs = Ap1 Abs
  signum = Ap1 Signum
