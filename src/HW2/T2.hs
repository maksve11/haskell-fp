module HW2.T2
    ( splitOn
    , joinWith
    ) where

import Data.List.NonEmpty (NonEmpty((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = error "Empty list is not allowed"
splitOn sep lst = case break (== sep) lst of
    (before, [])  -> before :| []
    (before, rest) -> case splitOn sep (tail rest) of
        x :| xs -> before :| (x : xs)

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (x :| xs) = foldl1 (\acc el -> acc ++ [sep] ++ el) (x:xs)
