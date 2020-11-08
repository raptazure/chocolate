module UsefulEquivalence (validUsefulEquivalences) where

import Logic (logEquiv1, logEquiv2, logEquiv3, (<=>), (==>))

validUsefulEquivalences =
  form1
    && form2a
    && form2b
    && form3a
    && form3b
    && form4a
    && form4b
    && form4c
    && form5a
    && form5b
    && form6
    && form7a
    && form7b
    && form8
    && form9a
    && form9b

-- law of double negation

form1 = logEquiv1 id (\p -> not (not p))

-- laws of idempotence

form2a = logEquiv1 id (\p -> p && p)

form2b = logEquiv1 id (\p -> p || p)

form3a = logEquiv2 (\p q -> p ==> q) (\p q -> not p || q)

form3b = logEquiv2 (\p q -> not (p ==> q)) (\p q -> p && (not q))

-- laws of contraposition

form4a = logEquiv2 (\p q -> not p ==> not q) (\p q -> q ==> p)

form4b = logEquiv2 (\p q -> p ==> not q) (\p q -> q ==> not p)

form4c = logEquiv2 (\p q -> not p ==> q) (\p q -> not q ==> p)

form5a = logEquiv2 (\p q -> p <=> q) (\p q -> (p ==> q) && (q ==> p))

form5b = logEquiv2 (\p q -> (p && q) || (not p && not q)) (\p q -> (p && q) || (not p && not q))

-- law of commutativity

form6 = logEquiv2 (\p q -> p && q) (\p q -> q && p)

-- DeMorgan laws

form7a = logEquiv2 (\p q -> not (p && q)) (\p q -> not p || not q)

form7b = logEquiv2 (\p q -> not (p || q)) (\p q -> not p && not q)

-- laws of associativity

form8 = logEquiv3 (\p q r -> p && (q && r)) (\p q r -> (p && q) && r)

-- distribution laws

form9a = logEquiv3 (\p q r -> p && (q || r)) (\p q r -> (p || q) && (p || r))

form9b = logEquiv3 (\p q r -> p || (q && r)) (\p q r -> (p && q) || (p && r))
