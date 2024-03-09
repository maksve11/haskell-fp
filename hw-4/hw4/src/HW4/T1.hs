{-# LANGUAGE LambdaCase #-}

module HW4.T1
  ( EvaluationError(..)
  , ExceptState(..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad (ap)
import HW4.Types

data ExceptState e s a = ES
  { runES :: s -> Except e (Annotated s a)
  }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES runSt) = ES (mapExcept (mapAnnotated f) . runSt)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState el = ES (\s -> Success $ el :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES es) =
  ES (joinExcept . mapExcept (\(st :# an) -> runES st an) . es)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (wrapExcept . (() :#) . f)

throwExceptState :: e -> ExceptState e s a
throwExceptState err = ES (\_ -> Error err)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) p q = ap p q

instance Monad (ExceptState e s) where
  (>>=) m f = joinExceptState (fmap f m)

data EvaluationError =
  DivideByZero
  deriving (Show, Eq)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval =
  \case
    Val x -> pure x
    Op (Add x y) -> evalBinOp x y (+) Add
    Op (Sub x y) -> evalBinOp x y (-) Sub
    Op (Mul x y) -> evalBinOp x y (*) Mul
    Op (Div x y) -> evalBinOp x y (/) Div
    Op (Abs x) -> evalUnoOp x abs Abs
    Op (Sgn x) -> evalUnoOp x signum Sgn

evalBinOp ::
     Expr
  -> Expr
  -> (Double -> Double -> Double)
  -> (Double -> Double -> Prim Double)
  -> ExceptState EvaluationError [Prim Double] Double
evalBinOp x y oper prim = do
  resX <- eval x
  resY <- eval y
  case prim resX resY of
    (Div _ 0) -> throwExceptState DivideByZero
    correctPrim -> do
      modifyExceptState (correctPrim :)
      pure (oper resX resY)

evalUnoOp ::
     Expr
  -> (Double -> Double)
  -> (Double -> Prim Double)
  -> ExceptState EvaluationError [Prim Double] Double
evalUnoOp x oper prim = do
  resX <- eval x
  modifyExceptState (prim resX :)
  pure (oper resX)