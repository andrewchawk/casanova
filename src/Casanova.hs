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
  deriving (Show)

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
  Limit String Expression
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
  Tan
  deriving (Show)

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
  deriving (Show)

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
