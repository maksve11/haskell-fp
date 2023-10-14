module HW2.T3
    ( mcat
    , epart
    ) where

import Data.Foldable
import Data.Monoid

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap (maybe mempty id)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap (\x -> (either id mempty x, either mempty id x))
