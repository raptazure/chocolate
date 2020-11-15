{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}

module Ring (Semiring, Ring, (><), fromInteger, abs, signum) where

import Groups (Group (invert), Monoid (mempty), Semigroup, (<>), (~~))
import Prelude (Bool (True), Int, Integer, Ord, (<=))
import qualified Prelude as Num

-- | Semiring
--
-- A semiring is an algebraic structure similar to a ring, but without the requirement that each element must have an additive inverse.
infixr 7 ><

class Semigroup r => Semiring r where
  (><) :: r -> r -> r

  fromBoolean :: Monoid r => Bool -> r
  fromBoolean _ = mempty

type Unital r = (Monoid r, Semiring r)

sunit :: Unital r => r
sunit = fromBoolean True

-- | Rings.
--
-- A ring /R/ is a commutative group with a second monoidal operation /></ that distributes over /<>/.
--
-- The basic properties of a ring follow immediately from the axioms:
--
-- @ r '><' 'mempty' ≡ 'mempty' ≡ 'mempty' '><' r @
--
-- @ 'negate' 'sunit' '><' r ≡ 'negate' r @
--
-- Furthermore, the binomial formula holds for any commuting pair of elements (that is, any /a/ and /b/ such that /a >< b = b >< a/).
--
-- If /mempty = sunit/ in a ring /R/, then /R/ has only one element, and is called the zero ring.
-- Otherwise the additive identity, the additive inverse of each element, and the multiplicative identity are unique.
--
-- See < https://en.wikipedia.org/wiki/Ring_(mathematics) >.
--
-- If the ring is < https://en.wikipedia.org/wiki/Ordered_ring ordered > (i.e. has an 'Ord' instance), then the following additional properties must hold:
--
-- @ a <= b ==> a <> c <= b <> c @
--
-- @ mempty <= a && mempty <= b ==> mempty <= a >< b @
--
-- See the properties module for a detailed specification of the laws.
class (Group r, Semiring r) => Ring r where
  -- | A ring homomorphism from the integers to /r/
  fromInteger :: Integer -> r

  -- | Absolute value of an element.
  --
  -- @ abs r ≡ r >< signum r @
  abs :: Ord r => r -> r
  abs x = if mempty <= x then sunit else invert sunit

  -- satisfies trichotomy law:
  -- Exactly one of the following is true: a is positive, -a is positive, or a = 0.
  -- This property follows from the fact that ordered rings are abelian, linearly ordered groups with respect to addition.
  signum :: Ord r => r -> r
  signum x = if mempty <= x then sunit else invert sunit

instance Semigroup Int where
  (<>) = (Num.+)

instance Monoid Int where
  mempty = 0

instance Group Int where
  (~~) = (Num.-)
  invert = Num.negate

instance Semiring Int where
  (><) = (Num.*)

instance Ring Int where
  fromInteger = Num.fromInteger
  abs = Num.abs
  signum = Num.signum
