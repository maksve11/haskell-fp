module HW1.T3
    ( Tree(..)
    , Meta
    , mkBranch
    , tsize
    , tdepth
    , tmember
    , tinsert
    , tFromList
    ) where

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
type Meta = Int

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l x r = Branch (1 + tsize l + tsize r) l x r

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch sz _ _ _) = sz

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ l _ r) = 1 + max (tdepth l) (tdepth r)

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l y r)
    | x == y = True
    | x < y = tmember x l
    | otherwise = tmember x r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = mkBranch Leaf x Leaf
tinsert x (Branch _ l y r)
    | x == y = Branch (tsize l + tsize r + 1) l x r
    | x < y = mkBranch (tinsert x l) y r
    | otherwise = mkBranch l y (tinsert x r)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf