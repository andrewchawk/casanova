import Data.Ratio
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
-- With the exception of complex numbers, as 'Complex' depends upon 'RealFloat',
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
  -- | This bit is used to represent complex numbers.  Go figure.
  --
  -- Some sort of 'Ratio'-based representation may eventually replace the
  -- 'Double' representation.  Such replacement would facilitate doing stuff
  -- with arbitrary-precision complex numbers *and* combining 'ExpRatio',
  -- 'ExpComplex', 'Infinity', and 'NegativeInfinity'.
  ExpComplex (Complex Double) |
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
  -- | Any such value is a lambda expression such that the name of the bound
  -- variable of this lambda expression and the body of this lambda expression
  -- are the 'String' value and the 'Expression' value, respectively.
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

-- | @simplify x@ is equivalent to @x@ but can be more easily computed.  The
-- simplification is mostly meant for calculation, as opposed to reading.  For
-- some values, @simplify@ may actually output a value which is /less/ readable.
simplify :: Expression -> Expression
simplify o = case o of
  Variable _ -> o
  ExpRatio _ -> o
  ExpComplex _ -> o
  Infinity -> o
  NegativeInfinity -> o
  Ap1 (Limit n x) m -> case m of
    Variable n2 -> if n2 == n then x else o
    Ap2 f a b -> simplify $ Ap2 f (Ap1 (Limit n x) a) (Ap1 (Limit n x) b)
    _ -> Ap1 (Limit n $ simplify x) $ simplify m
  Ap1 (Lambda n x) m -> simplify $ subst1 n m x
  Ap1 (Diff x) m -> case m of
    Infinity -> ExpRatio $ 0 % 1
    NegativeInfinity -> ExpRatio $ 0 % 1
    ExpRatio _ -> ExpRatio $ 0 % 1
    ExpComplex _ -> ExpRatio $ 0 % 1
    Variable y -> if y == x then ExpRatio (1 % 1) else o
    Ap1 Sin a -> simplify $ Ap1 Cos a
    Ap1 Cos a -> simplify $ Ap1 Negate $ Ap1 Sin a
    Ap1 Tan a -> simplify $ Ap1 Sec $ Ap1 Sec a
    Ap1 Csc a -> simplify $ Ap2 Product (Ap1 Negate $ Ap1 Cot a) (Ap1 Csc a)
    Ap1 Sec a -> simplify $ Ap2 Product (Ap1 Sec a) (Ap1 Tan a)
    Ap1 Cot a -> simplify $ Ap1 Negate $ square $ Ap1 Csc a
      where square x = Ap2 Exponent x $ ExpRatio $ 2 % 1
    Ap2 Product a b -> simplify $ Ap2 Sum (diff1 x a b) (diff1 x b a)
      where diff1 x2 a2 b2 = Ap2 Product (Ap1 (Diff x2) a2) b2
    _ -> Ap1 (Diff x) $ simplify m
  Ap1 f x -> Ap1 f $ simplify x
  Ap2 (Flip f) m n -> simplify $ Ap2 f n m
  Ap2 Sum (ExpRatio a) (ExpRatio b) -> ExpRatio $ a + b
  Ap2 Sum (ExpComplex a) (ExpComplex b) -> ExpComplex $ a + b
  Ap2 Sum Infinity NegativeInfinity -> ExpRatio $ 0 % 1
  Ap2 Sum a b
    | a == b -> Ap2 Product a $ ExpRatio $ 2 % 1
    | otherwise -> Ap2 Sum (simplify a) (simplify b)
  Ap2 Product (ExpRatio a) (ExpRatio b) -> ExpRatio $ a * b
  Ap2 Product (ExpComplex a) (ExpComplex b) -> ExpComplex $ a * b
  Ap2 Exponent (ExpRatio a) (ExpRatio b)
    | denominator a * denominator b == 1 -> ExpRatio $ (numerator a ^ numerator b) % 1
    | otherwise -> o
  Ap2 Exponent (ExpComplex a) (ExpComplex b) -> ExpComplex $ a ** b
  Ap2 f m n -> Ap2 f (simplify m) (simplify n)
