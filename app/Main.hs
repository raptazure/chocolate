module Main where

import Logic

-- ¬P ∧ ((P ⇒Q)⇔¬(Q ∧ ¬P))

p = True

q = False

formula1 = (not p) && (p ==> q) <=> not (q && (not p))

main :: IO ()
main = do
  print formula1
