module HW4.T1
    ( ExceptState (..)
    , mapExceptState
    , wrapExceptState
    , joinExceptState
    , modifyExceptState
    , throwExceptState
    , eval
    ) where

import HW3.T1 (Annotated(..), Except(..), mapAnnotated)
import HW3.T4 (Prim(..), Expr(..), State(..), modifyState)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }
data EvaluationError = DivideByZero

instance Functor (ExceptState e s) where
    fmap = mapExceptState

instance Applicative (ExceptState e s) where
    pure = wrapExceptState
    f <*> x = ES $ \s ->
        case runES f s of
            Error e -> Error e
            Success (a :# s') -> runES (mapExceptState a x) s'

instance Monad (ExceptState e s) where
    return = pure
    x >>= f = joinExceptState $ fmap f x

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s -> mapAnnotated f <$> runES es s

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState essa = ES $ \s ->
    case runES essa s of
        Error e -> Error e
        Success (g :# s') -> runES g s'

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval expr = do
    result <- evalState expr
    case result of
        Success (g :# history) -> do
            modifyExceptState (++ history)
            ES (\s -> wrapExceptState <$> runES g s)
        Error e -> throwExceptState e

evalState :: Expr -> ExceptState EvaluationError [Prim Double] Double
evalState expr = case expr of
    Val x -> wrapExceptState x
    Op (Add x y) -> do
        a <- evalState x
        b <- evalState y
        wrapExceptState (a + b)
    Op (Div x y) -> do
        a <- evalState x
        b <- evalState y
        if b /= 0
            then wrapExceptState (a / b)
            else throwExceptState DivideByZero