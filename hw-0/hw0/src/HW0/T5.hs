module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import GHC.Num (Natural)

type Nat a = (a -> a) -> a -> a

-- zero
nz :: Nat a
nz _ a = a

-- successor
ns :: Nat a -> Nat a
ns n f a = f (n f a)

-- addition and multiplication
nplus, nmult :: Nat a -> Nat a -> Nat a
nplus fL sL fR sR = fL fR (sL fR sR)
nmult fL sL fR = fL (sL fR)

-- conversion from natural
nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns (nFromNatural (n - 1))

-- conversion to num
nToNum :: Num a => Nat a -> a
nToNum a = a (+ 1) 0