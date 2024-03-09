{-# LANGUAGE LambdaCase #-}

module HW1.T2
  ( N(..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N
  = Z
  | S N

nplus :: N -> N -> N
nplus Z x     = x
nplus x Z     = x
nplus x (S y) = nplus (S x) y

nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult x (S y) = nplus x (nmult x y)

nsub :: N -> N -> Maybe N
nsub x Z         = Just x
nsub Z _         = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp _ Z         = GT
ncmp Z _         = LT
ncmp (S x) (S y) = ncmp x y

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S $ nFromNatural $ x - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = nToNum x + 1

nEven :: N -> Bool
nEven Z     = True
nEven (S Z) = False
nEven x     = nEven $ nmod x (S $ S Z)

nOdd :: N -> Bool
nOdd = not . nEven

maybeGet :: Maybe a -> a
maybeGet =
  \case
    Just x -> x
    Nothing ->
      error
        "Error!"

byNcmpResult :: N -> N -> N -> N -> (N -> N) -> N
byNcmpResult _ Z _ _ _ = error "Division by zero"
byNcmpResult x y resEq resLt fnGt =
  case ncmp x y of
    EQ -> resEq
    LT -> resLt
    GT -> fnGt $ maybeGet $ nsub x y

ndiv :: N -> N -> N
ndiv x y = byNcmpResult x y (S Z) Z (\t -> nplus (ndiv t y) (S Z))

nmod :: N -> N -> N
nmod x y = byNcmpResult x y Z x (`nmod` y)