import Data.Ratio
import Data.Complex

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
  -- | @ExpInt x@ is the representation of @x@.
  ExpInt Integer |
  -- This bit is used to represent fractions.
  ExpRatio (Ratio Integer) |
  -- | This bit is used to represent complex integers.  Go figure.
  ExpComplex (Complex Integer) |
  -- | 'Infinity' is just infinity.  This value behaves like the infinity of the
  -- floating-point numbers.
  Infinity |
  -- | This value behaves like the negative infinity of the floating-point
  -- numbers.
  NegativeInfinity
  deriving (Show)
