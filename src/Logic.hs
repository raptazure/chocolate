module Logic
  ( (==>),
    (<=>),
    (<+>),
    valid1,
    valid2,
    valid3,
    valid4,
  )
where

{- Logical Connectives -}

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

-- ∧ and ∨ bind more strongly than ⇒ and ⇔
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

{- Logic Validity -}

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && (bf False)

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =
  (bf True True)
    && (bf True False)
    && (bf True False)
    && (bf False True)
    && (bf False False)

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf =
  and
    [ bf p q r
      | p <- [True, False],
        q <- [True, False],
        r <- [True, False]
    ]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf =
  and
    [ bf p q r s
      | p <- [True, False],
        q <- [True, False],
        r <- [True, False],
        s <- [True, False]
    ]
