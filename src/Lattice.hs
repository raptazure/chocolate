{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Lattice
  ( -- * Unbounded lattices
    Lattice (..),
    joinLeq,
    meetLeq,

    -- * Bounded lattices
    BoundedJoinSemiLattice (..),
    BoundedMeetSemiLattice (..),
    joins,
    meets,
    fromBool,
    BoundedLattice,

    -- * Monoid wrappers
    Meet (..),
    Join (..),

    -- * Fixed points of chains in lattices
    lfp,
    lfpFrom,
    unsafeLfp,
    gfp,
    gfpFrom,
    unsafeGfp,

    -- * PartialOrd
    PartialOrd,
    comparable,
  )
where

import Data.Data (Data, Typeable)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (..))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Universe.Class (Finite, universeF)
import GHC.Generics (Generic)

infixr 6 /\

infixr 5 \/

-- | An algebraic structure with joins and meets.
--
-- See <http://en.wikipedia.org/wiki/Lattice_(order)> and <http://en.wikipedia.org/wiki/Absorption_law>.
--
-- 'Lattice' is very symmetric, which is seen from the laws:
--
-- /Associativity/
--
-- @
-- x '\/' (y '\/' z) ≡ (x '\/' y) '\/' z
-- x '/\' (y '/\' z) ≡ (x '/\' y) '/\' z
-- @
--
-- /Commputativity/
--
-- @
-- x '\/' y ≡ y '\/' x
-- x '/\' y ≡ y '/\' x
-- @
--
-- /Idempotency/
--
-- @
-- x '\/' x ≡ x
-- x '/\' x ≡ x
-- @
--
-- /Absorption/
--
-- @
-- a '\/' (a '/\' b) ≡ a
-- a '/\' (a '\/' b) ≡ a
-- @
class Lattice a where
  -- | Join
  (\/) :: a -> a -> a

  -- | meet
  (/\) :: a -> a -> a

-- | The partial ordering induced by the join-semilattice structure
joinLeq :: (Eq a, Lattice a) => a -> a -> Bool
joinLeq x y = (x \/ y) == y

meetLeq :: (Eq a, Lattice a) => a -> a -> Bool
meetLeq x y = (x /\ y) == x

-- | A join-semilattice with an identity element 'bottom' for '\/'.
--
-- /Laws/
--
-- @
-- x '\/' 'bottom' ≡ x
-- @
--
-- /Corollary/
--
-- @
-- x '/\' 'bottom'
--   ≡⟨ identity ⟩
-- (x '/\' 'bottom') '\/' 'bottom'
--   ≡⟨ absorption ⟩
-- 'bottom'
-- @
class Lattice a => BoundedJoinSemiLattice a where
  bottom :: a

-- | The join of a list of join-semilattice elements
joins :: (BoundedJoinSemiLattice a, Foldable f) => f a -> a
joins = getJoin . foldMap Join

-- | A meet-semilattice with an identity element 'top' for '/\'.
--
-- /Laws/
--
-- @
-- x '/\' 'top' ≡ x
-- @
--
-- /Corollary/
--
-- @
-- x '\/' 'top'
--   ≡⟨ identity ⟩
-- (x '\/' 'top') '/\' 'top'
--   ≡⟨ absorption ⟩
-- 'top'
-- @
class Lattice a => BoundedMeetSemiLattice a where
  top :: a

-- | The meet of a list of meet-semilattice elements
meets :: (BoundedMeetSemiLattice a, Foldable f) => f a -> a
meets = getMeet . foldMap Meet

type BoundedLattice a = (BoundedMeetSemiLattice a, BoundedJoinSemiLattice a)

-- | 'True' to 'top' and 'False' to 'bottom'
fromBool :: BoundedLattice a => Bool -> a
fromBool True = top
fromBool False = bottom

--
-- Sets
--

instance Ord a => Lattice (Set.Set a) where
  (\/) = Set.union
  (/\) = Set.intersection

instance Ord a => BoundedJoinSemiLattice (Set.Set a) where
  bottom = Set.empty

instance (Ord a, Finite a) => BoundedMeetSemiLattice (Set.Set a) where
  top = Set.fromList universeF

--
-- IntSets
--

instance Lattice IS.IntSet where
  (\/) = IS.union
  (/\) = IS.intersection

instance BoundedJoinSemiLattice IS.IntSet where
  bottom = IS.empty

--
-- HashSet
--

instance (Eq a, Hashable a) => Lattice (HS.HashSet a) where
  (\/) = HS.union
  (/\) = HS.intersection

instance (Eq a, Hashable a) => BoundedJoinSemiLattice (HS.HashSet a) where
  bottom = HS.empty

instance (Eq a, Hashable a, Finite a) => BoundedMeetSemiLattice (HS.HashSet a) where
  top = HS.fromList universeF

--
-- Maps
--

instance (Ord k, Lattice v) => Lattice (Map.Map k v) where
  (\/) = Map.unionWith (\/)
  (/\) = Map.intersectionWith (/\)

instance (Ord k, Lattice v) => BoundedJoinSemiLattice (Map.Map k v) where
  bottom = Map.empty

instance (Ord k, Finite k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (Map.Map k v) where
  top = Map.fromList (universeF `zip` repeat top)

--
-- IntMaps
--

instance Lattice v => Lattice (IM.IntMap v) where
  (\/) = IM.unionWith (\/)
  (/\) = IM.intersectionWith (/\)

instance Lattice v => BoundedJoinSemiLattice (IM.IntMap v) where
  bottom = IM.empty

--
-- HashMaps
--

instance (Eq k, Hashable k, Lattice v) => BoundedJoinSemiLattice (HM.HashMap k v) where
  bottom = HM.empty

instance (Eq k, Hashable k, Lattice v) => Lattice (HM.HashMap k v) where
  (\/) = HM.unionWith (\/)
  (/\) = HM.intersectionWith (/\)

instance (Eq k, Hashable k, Finite k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (HM.HashMap k v) where
  top = HM.fromList (universeF `zip` repeat top)

--
-- Functions
--

instance Lattice v => Lattice (k -> v) where
  f \/ g = \x -> f x \/ g x
  f /\ g = \x -> f x /\ g x

instance BoundedJoinSemiLattice v => BoundedJoinSemiLattice (k -> v) where
  bottom = const bottom

instance BoundedMeetSemiLattice v => BoundedMeetSemiLattice (k -> v) where
  top = const top

--
-- Unit
--

instance Lattice () where
  _ \/ _ = ()
  _ /\ _ = ()

instance BoundedJoinSemiLattice () where
  bottom = ()

instance BoundedMeetSemiLattice () where
  top = ()

--
-- Tuples
--

instance (Lattice a, Lattice b) => Lattice (a, b) where
  (x1, y1) \/ (x2, y2) = (x1 \/ x2, y1 \/ y2)
  (x1, y1) /\ (x2, y2) = (x1 /\ x2, y1 /\ y2)

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (a, b) where
  bottom = (bottom, bottom)

instance (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (a, b) where
  top = (top, top)

--
-- Bools
--

instance Lattice Bool where
  (\/) = (||)
  (/\) = (&&)

instance BoundedJoinSemiLattice Bool where
  bottom = False

instance BoundedMeetSemiLattice Bool where
  top = True

-- | A partial ordering on sets
-- (<http://en.wikipedia.org/wiki/Partially_ordered_set>) is a set equipped
-- with a binary relation, `leq`, that obeys the following laws
--
-- @
-- Reflexive:     a ``leq`` a
-- Antisymmetric: a ``leq`` b && b ``leq`` a ==> a == b
-- Transitive:    a ``leq`` b && b ``leq`` c ==> a ``leq`` c
-- @
--
-- Two elements of the set are said to be `comparable` when they are are
-- ordered with respect to the `leq` relation. So
--
-- @
-- `comparable` a b ==> a ``leq`` b || b ``leq`` a
-- @
--
-- If `comparable` always returns true then the relation `leq` defines a
-- total ordering (and an `Ord` instance may be defined). Any `Ord` instance is
-- trivially an instance of `PartialOrd`. 'Algebra.Lattice.Ordered' provides a
-- convenient wrapper to satisfy 'PartialOrd' given 'Ord'.
--
-- As an example consider the partial ordering on sets induced by set
-- inclusion.  Then for sets `a` and `b`,
--
-- @
-- a ``leq`` b
-- @
--
-- is true when `a` is a subset of `b`.  Two sets are `comparable` if one is a
-- subset of the other. Concretely
--
-- @
-- a = {1, 2, 3}
-- b = {1, 3, 4}
-- c = {1, 2}
--
-- a ``leq`` a = `True`
-- a ``leq`` b = `False`
-- a ``leq`` c = `False`
-- b ``leq`` a = `False`
-- b ``leq`` b = `True`
-- b ``leq`` c = `False`
-- c ``leq`` a = `True`
-- c ``leq`` b = `False`
-- c ``leq`` c = `True`
--
-- `comparable` a b = `False`
-- `comparable` a c = `True`
-- `comparable` b c = `False`
-- @
class Eq a => PartialOrd a where
  -- | The relation that induces the partial ordering
  leq :: a -> a -> Bool

  -- | Whether two elements are ordered with respect to the relation. A
  -- default implementation is given by
  --
  -- @
  -- 'comparable' x y = 'leq' x y '||' 'leq' y x
  -- @
  comparable :: a -> a -> Bool
  comparable x y = leq x y || leq y x

-- | Monoid wrapper for join-'Lattice'
newtype Join a = Join {getJoin :: a}
  deriving (Eq, Ord, Read, Show, Bounded, Typeable, Data, Generic)

instance Lattice a => Semigroup (Join a) where
  Join a <> Join b = Join (a \/ b)

instance BoundedJoinSemiLattice a => Monoid (Join a) where
  mempty = Join bottom
  Join a `mappend` Join b = Join (a \/ b)

instance (Eq a, Lattice a) => PartialOrd (Join a) where
  leq (Join a) (Join b) = joinLeq a b

instance Functor Join where
  fmap f (Join x) = Join (f x)

instance Applicative Join where
  pure = Join
  Join f <*> Join x = Join (f x)
  _ *> x = x

instance Monad Join where
  return = pure
  Join m >>= f = f m
  (>>) = (*>)

-- | Monoid wrapper for meet-'Lattice'
newtype Meet a = Meet {getMeet :: a}
  deriving (Eq, Ord, Read, Show, Bounded, Typeable, Data, Generic)

instance Lattice a => Semigroup (Meet a) where
  Meet a <> Meet b = Meet (a /\ b)

instance BoundedMeetSemiLattice a => Monoid (Meet a) where
  mempty = Meet top
  Meet a `mappend` Meet b = Meet (a /\ b)

instance (Eq a, Lattice a) => PartialOrd (Meet a) where
  leq (Meet a) (Meet b) = meetLeq a b

instance Functor Meet where
  fmap f (Meet x) = Meet (f x)

instance Applicative Meet where
  pure = Meet
  Meet f <*> Meet x = Meet (f x)
  _ *> x = x

instance Monad Meet where
  return = pure
  Meet m >>= f = f m
  (>>) = (*>)

-------------------------------------------------------------------------------
-- Theorems
-------------------------------------------------------------------------------

-- | Least point of a partially ordered monotone function. Does not checks that the function is monotone.
unsafeLfpFrom :: Eq a => a -> (a -> a) -> a
unsafeLfpFrom = lfpFrom' (\_ _ -> True)

{-# INLINE lfpFrom' #-}
lfpFrom' :: Eq a => (a -> a -> Bool) -> a -> (a -> a) -> a
lfpFrom' check init_x f = go init_x
  where
    go x
      | x' == x = x
      | x `check` x' = go x'
      | otherwise = error "lfpFrom: non-monotone function"
      where
        x' = f x

-- | Greatest fixed point of a partially ordered antinone function. Does not check that the function is antinone.
{-# INLINE unsafeGfpFrom #-}
unsafeGfpFrom :: Eq a => a -> (a -> a) -> a
unsafeGfpFrom = gfpFrom' (\_ _ -> True)

{-# INLINE gfpFrom' #-}
gfpFrom' :: Eq a => (a -> a -> Bool) -> a -> (a -> a) -> a
gfpFrom' check init_x f = go init_x
  where
    go x
      | x' == x = x
      | x' `check` x = go x'
      | otherwise = error "gfpFrom: non-antinone function"
      where
        x' = f x

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Assumes that the function is monotone and does not check if that is correct.
{-# INLINE unsafeLfp #-}
unsafeLfp :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
unsafeLfp = unsafeLfpFrom bottom

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be monotone.
{-# INLINE lfp #-}
lfp :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
lfp = lfpFrom bottom

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be monotone.
{-# INLINE lfpFrom #-}
lfpFrom :: (Eq a, BoundedJoinSemiLattice a) => a -> (a -> a) -> a
lfpFrom init_x f = unsafeLfpFrom init_x (\x -> f x \/ x)

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Assumes that the function is antinone and does not check if that is correct.
{-# INLINE unsafeGfp #-}
unsafeGfp :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
unsafeGfp = unsafeGfpFrom top

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be antinone.
{-# INLINE gfp #-}
gfp :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
gfp = gfpFrom top

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be antinone.
{-# INLINE gfpFrom #-}
gfpFrom :: (Eq a, BoundedMeetSemiLattice a) => a -> (a -> a) -> a
gfpFrom init_x f = unsafeGfpFrom init_x (\x -> f x /\ x)
