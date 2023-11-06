module HW3.T2 where

import HW3.T1

-- Implement the distF function for each data type

-- Option
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

-- Pair
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

-- Quad
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

-- Annotated
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a1 :# e1, a2 :# e2) = (a1, a2) :# (e1 <> e2)

-- Except
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e1, _) = Error e1
distExcept (_, Error e2) = Error e2
distExcept (Success a1, Success a2) = Success (a1, a2)

-- Prioritised
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a1, High a2) = High (a1, a2)
distPrioritised (Medium a, High b) = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Low a, High b) = High (a, b)
distPrioritised (Low a, Medium b) = Medium (a, b)
distPrioritised (Low a1, Low a2) = Low (a1, a2)

-- Stream
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a1 :> rest1, a2 :> rest2) = (a1, a2) :> distStream (rest1, rest2)

-- List
distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a1 :. rest1, list2) = combineList a1 list2 `appendList` distList (rest1, list2)
    where
        combineList _ Nil = Nil
        combineList a (b :. rest) = (a, b) :. combineList a rest

-- Helper function to append two lists
appendList :: List (a, b) -> List (a, b) -> List (a, b)
appendList Nil list = list
appendList (a :. rest1) list2 = a :. appendList rest1 list2

-- Fun
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F g1, F g2) = F (\i -> (g1 i, g2 i))

-- Implement the wrapF function for each data type

-- Option
wrapOption :: a -> Option a
wrapOption a = Some a

-- Pair
wrapPair :: a -> Pair a
wrapPair a = P a a

-- Quad
wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

-- Annotated
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- Except
wrapExcept :: a -> Except e a
wrapExcept a = Success a

-- Prioritised
wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = High a

-- Stream
wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

-- List
wrapList :: a -> List a
wrapList a = a :. Nil

-- Fun
wrapFun :: a -> Fun i a
wrapFun a = F (\_ -> a)