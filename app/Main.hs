module Main where

import Logic
import UsefulEquivalence

-- ¬P ∧ ((P ⇒Q)⇔¬(Q ∧ ¬P))

formula1 :: Bool -> Bool -> Bool
formula1 p q = (not p) && (p ==> q) <=> not (q && (not p))

excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

formula2 p q = p ==> (q ==> p)

formula3 p q = (p ==> q) ==> p

formula4 p q r = (p || q) ==> r

formula5 p q r = (p ==> r) && (q ==> r)

formula6 p q r = ((p || q) ==> r) <=> ((p ==> r) && (q ==> r))

main :: IO ()
main = do
  print $ formula1 True False
  print $ valid1 excluded_middle
  print $ valid2 formula2
  print $ valid2 formula3
  print $ (logEquiv3 formula4 formula5) == (valid3 formula6)
  print validUsefulEquivalences
