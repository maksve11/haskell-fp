module HW3.T4
    ( State(..)
    , Prim(..)
    , Expr(..)
    , mapState
    , wrapState
    , joinState
    , modifyState
    , eval
    )
where
    
import HW3.T1

-- Define the State data type
data State s a = S { runS :: s -> Annotated s a }

-- Implement Functor instance for State
instance Functor (State s) where
    fmap f state = S $ \s -> let (a :# s') = runS state s in (f a) :# s'

-- Implement Applicative instance for State
instance Applicative (State s) where
    pure a = S $ \s -> a :# s
    stateF <*> stateA = S $ \s ->
        let (f :# s') = runS stateF s
            (a :# s'') = runS stateA s'
        in (f a) :# s''

-- Implement Monad instance for State
instance Monad (State s) where
    return = pure
    state >>= f = S $ \s ->
        let (a :# s') = runS state s
            (b :# s'') = runS (f a) s'
        in b :# s''

-- Implement the remaining functions mapState, wrapState, joinState, and modifyState
mapState :: (a -> b) -> State s a -> State s b
mapState f state = do
    a <- state
    return (f a)

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState state = S $ \s ->
    let (innerState :# s') = runS state s
    in runS innerState s'

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

-- Define the Prim data type
data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a

-- Define the Expr data type
data Expr = Val Double | Op (Prim Expr)

-- Define Num and Fractional instances for Expr
instance Num Expr where
    x + y = Op (Add x y)
    x - y = Op (Sub x y)
    x * y = Op (Mul x y)
    fromInteger x = Val (fromInteger x)
    abs _ = undefined 
    signum _ = undefined

instance Fractional Expr where
    x / y = Op (Div x y)
    fromRational x = Val (fromRational x)

-- Define the eval function for evaluating Expr using the State monad
eval :: Expr -> State [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) = do
    a <- eval x
    b <- eval y
    modifyState (Add a b :)
    return (a + b)
eval (Op (Sub x y)) = do
    a <- eval x
    b <- eval y
    modifyState (Sub a b :)
    return (a - b)
eval (Op (Mul x y)) = do
    a <- eval x
    b <- eval y
    modifyState (Mul a b :)
    return (a * b)
eval (Op (Div x y)) = do
    a <- eval x
    b <- eval y
    modifyState (Div a b :)
    return (a / b)
eval (Op (Abs x)) = do
    a <- eval x
    modifyState (Abs a :)
    return (abs a)
eval (Op (Sgn x)) = do
    a <- eval x
    modifyState (Sgn a :)
    return (signum a)