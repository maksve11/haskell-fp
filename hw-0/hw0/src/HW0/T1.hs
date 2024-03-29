{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

-- distrib function
distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

-- assocPair isomorphism
assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

-- assocEither isomorphism
assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso (\case
                      Left a -> Left (Left a)
                      Right (Left b) -> Left (Right b)
                      Right (Right c) -> Right c)
                 (\case
                      Left (Left a) -> Left a
                      Left (Right b) -> Right (Left b)
                      Right c -> Right (Right c))
