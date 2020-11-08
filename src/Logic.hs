module Logic
  ( (==>),
    (<=>),
    (<+>),
  )
where

-- and ∧ conjunction
-- or ∨ disjunction
-- not ¬ negation
-- if-then ⇒ implication
-- if, and only if ⇔ equivalence

-- (&&) :: Bool -> Bool -> Bool
-- False && x = False
-- True && x = x

-- (||) :: Bool -> Bool -> Bool
-- False || x = x
-- True || x = True

-- not :: Bool -> Bool
-- not True = False
-- not False = True

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
-- an implication should be false in the only remaining case that the antecedent is true and the consequent false
x ==> y = (not x) || y

-- or
-- (==>) :: Bool -> Bool -> Bool
-- True ==> x = x
-- False ==> x = True

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

-- exclusive or ⊕, ¬(P ⇔ Q)
infixr 2 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y
