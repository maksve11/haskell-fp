module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a]
repeat' a = fix (a :)

map' :: (a -> b) -> [a] -> [b]
map' =
  fix
    ( \rec f l ->
        if null l
          then []
          else f (head l) : rec f (tail l)
    )

fib :: Natural -> Natural
fib n = fib' n (0, 1)
  where
    fib' 0 (a, _) = a
    fib' n (a, b) = fib' (n - 1) (b, a + b)

fac :: Natural -> Natural
fac = fix (\rec n -> if n == 0 then 1 else n * rec (n - 1))