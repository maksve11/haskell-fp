{-# LANGUAGE LambdaCase #-}

module HW2.T4
  ( DotString(..)
  , Fun(..)
  , Inclusive(..)
  , ListPlus(..)
  ) where

data ListPlus a
  = a :+ ListPlus a
  | Last a
  deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) =
    \case
      Last s -> (:+) s
      (s :+ sx) -> (:+) s . (<>) sx

data Inclusive a b
  = This a
  | That b
  | Both a b
  deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This x) (This y)     = This (x <> y)
  (<>) (This x) (That y)     = Both x y
  (<>) (This x) (Both y t)   = Both (x <> y) t
  (<>) (That x) (This y)     = Both y x
  (<>) (That x) (That y)     = That (x <> y)
  (<>) (That x) (Both y t)   = Both y (x <> t)
  (<>) (Both x y) (This t)   = Both (x <> t) y
  (<>) (Both x y) (That t)   = Both x (y <> t)
  (<>) (Both x y) (Both t v) = Both (x <> t) (y <> v)

newtype DotString =
  DS String
  deriving (Show)

instance Semigroup DotString where
  (<>) (DS first) (DS second)
    | first == mempty = DS second
    | second == mempty = DS first
    | otherwise = DS $ concat [first, ".", second]

instance Monoid DotString where
  mempty = DS ""

newtype Fun a =
  F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F func1) (F func2) = F (func1 . func2)

instance Monoid (Fun a) where
  mempty = F id