-- | This module contains the types from hw3 that are also
-- needed for hw4.
{-# LANGUAGE LambdaCase #-}

module HW4.Types
  ( Annotated(..)
  , Except(..)
  , Expr(..)
  , Prim(..)
  , State(..)
  , mapExcept
  , wrapExcept
  , joinExcept
  , mapAnnotated
  ) where

data Except e a
  = Error e
  | Success a
  deriving (Show, Eq)

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f =
  \case
    Success x -> Success (f x)
    Error y -> Error y

wrapExcept :: a -> Except e a
wrapExcept = Success

joinExcept :: Except e (Except e a) -> Except e a
joinExcept =
  \case
    Success x -> x
    Error e -> Error e

data Annotated e a =
  a :# e
  deriving (Show, Eq)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

data State s a = S
  { runS :: s -> Annotated s a
  }

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show, Eq)

data Expr
  = Val Double
  | Op (Prim Expr)
  deriving (Show, Eq)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)