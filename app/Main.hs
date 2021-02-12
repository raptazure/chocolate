module Main where

import Groups
  ( Group (invert),
    Product (Product, getProduct),
    Sum (Sum, getSum),
  )
import Logic
  ( every,
    logEquiv3,
    some,
    valid1,
    valid2,
    valid3,
    (<=>),
    (==>),
  )
import UsefulEquivalence (validUsefulEquivalences)

-- ¬P ∧ ((P ⇒Q)⇔¬(Q ∧ ¬P))

formula1 :: Bool -> Bool -> Bool
formula1 p q = not p && (p ==> q) <=> not (q && not p)

excludedMiddle :: Bool -> Bool
excludedMiddle p = p || not p

formula2 :: Bool -> Bool -> Bool
formula2 p q = p ==> (q ==> p)

formula3 :: Bool -> Bool -> Bool
formula3 p q = (p ==> q) ==> p

formula4 :: Bool -> Bool -> Bool -> Bool
formula4 p q r = (p || q) ==> r

formula5 :: Bool -> Bool -> Bool -> Bool
formula5 p q r = (p ==> r) && (q ==> r)

formula6 :: Bool -> Bool -> Bool -> Bool
formula6 p q r = ((p || q) ==> r) <=> ((p ==> r) && (q ==> r))

-- ∀x ∈ {1, 4, 9}∃y ∈ {1, 2, 3} x = y2
formula7 :: Bool
formula7 = every [1, 4, 9] (\x -> some [1, 2, 3] (\y -> x == y ^ 2))

main :: IO ()
main = do
  -- solving A + B
  -- (sum <$> (map read . words) `fmap` getLine) >>= (\a -> print a)
  print $ formula1 True False
  print $ valid1 excludedMiddle
  print $ valid2 formula2
  print $ valid2 formula3
  print $ logEquiv3 formula4 formula5 == valid3 formula6
  print validUsefulEquivalences
  print formula7
  print $ getSum $ invert $ Sum (-1)
  print $ getProduct $ invert $ Product (3 / 4)
