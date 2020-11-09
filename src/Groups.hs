module Groups
  ( Semigroup,
    Monoid,
    (<>),
    mempty,
    mappend,
    mconcat,
    (~~),
    pow,
    Abelian,
    Cyclic,
    generator,
    Sum (..),
    Product (..),
    invert,
  )
where

import Prelude hiding (Monoid, Semigroup, mappend, mconcat, mempty, (<>))

{- Semigroup -}
class Semigroup a where
  -- Associativity
  -- (a <> b) <> c == a <> (b <> c)

  -- defining sconcat and stimes is unnecessary (default implementation)
  -- sconcat :: NonEmpty m -> m
  -- stimes :: Integral a => a -> m -> m
  (<>) :: a -> a -> a

{- Monoid -}
class Semigroup m => Monoid m where
  -- Identity laws
  -- x <> mempty = x
  -- mempty <> x = x

  -- Associativity
  -- (x <> y) <> z = x <> (y <> z)

  mempty :: m

  -- defining mappend is unnecessary, it copies from Semigroup
  mappend :: m -> m -> m
  mappend = (<>)

  -- defining mconcat is optional, since it has the following default:
  mconcat :: [m] -> m
  mconcat = foldl mappend mempty

{- Group -}
class Monoid m => Group m where
  -- a Monoid plus a function, invert, such that:
  -- a <> invert a == mempty
  -- invert a <> a == mempty
  invert :: m -> m
  (~~) :: m -> m -> m
  x ~~ y = x `mappend` invert y

  -- If n is negative, the result is inverted.
  pow :: Integral x => m -> x -> m
  pow x0 n0 = case compare n0 0 of
    LT -> invert . f x0 $ negate n0
    EQ -> mempty
    GT -> f x0 n0
    where
      f x n
        | even n = f (x `mappend` x) (n `quot` 2)
        | n == 1 = x
        | otherwise = g (x `mappend` x) (n `quot` 2) x
      g x n c
        | even n = g (x `mappend` x) (n `quot` 2) c
        | n == 1 = x `mappend` c
        | otherwise = g (x `mappend` x) (n `quot` 2) (x `mappend` c)

{- Abelian -}
class Group g => Abelian g

-- An Abelian group is a Group that follows the rule: a <> b == b <> a

{- Cyclic -}
class Group g => Cyclic g where
  -- A Group G is Cyclic if there exists an element x of G such that for all y in G, there exists an n, such that: y = pow x n
  generator :: g

{- Examples -}
instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

newtype Sum n = Sum {getSum :: n}

instance Num n => Semigroup (Sum n) where
  Sum x <> Sum y = Sum (x + y)

instance Num n => Monoid (Sum n) where
  mempty = Sum 0

instance Num n => Group (Sum n) where
  invert = Sum . negate . getSum
  {-# INLINE invert #-}
  pow (Sum a) b = Sum (a * fromIntegral b)

instance Num n => Abelian (Sum n)

newtype Product n = Product {getProduct :: n}

instance Num n => Semigroup (Product n) where
  Product x <> Product y = Product (x * y)

instance Num n => Monoid (Product n) where
  mempty = Product 1

instance Fractional a => Group (Product a) where
  invert = Product . recip . getProduct
  {-# INLINE invert #-}
  pow (Product a) b = Product (a ^^ b)

instance Fractional a => Abelian (Product a)
