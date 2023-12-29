module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

k :: a -> b -> a
k x y = x

i :: a -> a
i a = k a a

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose bc ab a = bc (ab a)

contract :: (a -> a -> b) -> (a -> b)
contract aab = s aab i

permute :: (a -> b -> c) -> (b -> a -> c)
permute abc b a = abc a b