module HW1.T2
    ( N(..)
    , nplus
    , nmult
    , nsub
    , ncmp
    , nFromNatural
    , nToNum
    , nEven
    , nOdd
    , ndiv
    , nmod
    ) where

import Numeric.Natural as Natural 

data N = Z | S N

nplus :: N -> N -> N
nplus Z n = n
nplus (S m) n = S (nplus m n)

nmult :: N -> N -> N
nmult Z _ = Z
nmult (S m) n = nplus n (nmult m n)

nsub :: N -> N -> Maybe N
nsub Z _ = Just Z
nsub m Z = Just m
nsub (S m) (S n) = case nsub m n of
    Just res -> Just (S res)
    Nothing -> Nothing

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S m) (S n) = ncmp m n

-- | Convert a Natural number to N.
nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool
nEven Z = True
nEven (S n) = nOdd n

nOdd :: N -> Bool
nOdd Z = False
nOdd (S n) = nEven n

ndiv :: N -> N -> N
ndiv _ Z = undefined
ndiv Z _ = Z
ndiv m n = case nsub m n of
    Just res -> S (ndiv res n)
    Nothing -> undefined

nmod :: N -> N -> N
nmod _ Z = undefined
nmod m n = case nsub m (nmult (ndiv m n) n) of
    Just res -> res
    Nothing -> undefined