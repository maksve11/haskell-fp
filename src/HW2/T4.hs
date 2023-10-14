module HW2.T4
    ( ListPlus(..)
    , DotString(..)
    , Fun(..)
    ) where

import Data.Semigroup (Last)

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+

instance Semigroup (ListPlus a) where
    (Last x) <> ys = x :+ ys
    (a :+ as) <> bs = a :+ (as <> bs)

newtype DotString = DS String

instance Show DotString where
    show (DS s) = show s

instance Semigroup DotString where
    (DS a) <> (DS b) = DS (a ++ "." ++ b)

instance Monoid DotString where
    mempty = DS ""
    mappend = (<>)

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
    (F f) <> (F g) = F (f . g)

instance Monoid (Fun a) where
    mempty = F id