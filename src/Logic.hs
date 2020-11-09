module Logic
  ( (==>),
    (<=>),
    (<+>),
    valid1,
    valid2,
    valid3,
    valid4,
    logEquiv1,
    logEquiv2,
    logEquiv3,
    every,
    some,
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

{- Logical Equivalence -}

-- test the formula Φ ⇔ Ψ by the truth table method
logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =
  (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

logEquiv2 ::
  (Bool -> Bool -> Bool) ->
  (Bool -> Bool -> Bool) ->
  Bool
logEquiv2 bf1 bf2 =
  and
    [ (bf1 p q) <=> (bf2 p q)
      | p <- [True, False],
        q <- [True, False]
    ]

logEquiv3 ::
  (Bool -> Bool -> Bool -> Bool) ->
  (Bool -> Bool -> Bool -> Bool) ->
  Bool
logEquiv3 bf1 bf2 =
  and
    [ (bf1 p q r) <=> (bf2 p q r)
      | p <- [True, False],
        q <- [True, False],
        r <- [True, False]
    ]

{- quantifier procedures -}

every, some :: [a] -> (a -> Bool) -> Bool
every xs p = all p xs
some xs p = any p xs
