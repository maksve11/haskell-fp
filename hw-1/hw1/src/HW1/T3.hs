{-# LANGUAGE LambdaCase #-}

module HW1.T3
  ( Tree(..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta =
  M Int Int

data Tree a
  = Leaf
  | Branch Meta (Tree a) a (Tree a)

tsize :: Tree a -> Int
tsize =
  \case
    Leaf -> 0
    Branch (M size _) _ _ _ -> size

tdepth :: Tree a -> Int
tdepth =
  \case
    Leaf -> 0
    Branch (M _ height) _ _ _ -> height

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ left y right) =
  case compare x y of
    EQ -> True
    LT -> tmember x left
    GT -> tmember x right

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x tree =
  case tree of
    Leaf -> Branch (M 1 1) Leaf x Leaf
    (Branch _ left v right) ->
      case compare x v of
        EQ -> tree
        LT -> balance (constructTree v (tinsert x left) right)
        GT -> balance (constructTree v left (tinsert x right))

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf

balance :: Tree a -> Tree a
balance tree =
  case getBalance tree of
    2 -> handleLeftRotate tree
    2 -> handleRightRotate tree
    _ -> tree

handleLeftRotate :: Tree a -> Tree a
handleLeftRotate tree =
  case tree of
    Leaf -> Leaf
    (Branch m left v right) ->
      if getBalance right == 1
        then bigLeftRotate (Branch m left v right)
        else leftRotate (Branch m left v right)

bigLeftRotate :: Tree a -> Tree a
bigLeftRotate (Branch _ left v right) =
  leftRotate (constructTree v left (rightRotate right))
bigLeftRotate otherCase = otherCase

leftRotate :: Tree a -> Tree a
leftRotate (Branch _ left v (Branch _ rl vl rr)) =
  constructTree vl (constructTree v left rl) rr
leftRotate otherCase = otherCase

handleRightRotate :: Tree a -> Tree a
handleRightRotate =
  \case
    Leaf -> Leaf
    (Branch m left v right) ->
      if getBalance left == -1
        then bigRightRotate (Branch m left v right)
        else rightRotate (Branch m left v right)

bigRightRotate :: Tree a -> Tree a
bigRightRotate (Branch _ left v right) =
  rightRotate (constructTree v (leftRotate left) right)
bigRightRotate otherCase = otherCase

rightRotate :: Tree a -> Tree a
rightRotate (Branch _ (Branch _ ll vl lr) v right) =
  constructTree vl ll (constructTree v lr right)
rightRotate otherCase = otherCase

constructTree :: a -> Tree a -> Tree a -> Tree a
constructTree v left right = Branch (newMetaByChildren left right) left v right

getBalance :: Tree a -> Int
getBalance =
  \case
    Leaf -> 0
    (Branch _ left _ right) -> tdepth left - tdepth right

newMetaByChildren :: Tree a -> Tree a -> Meta
newMetaByChildren left right =
  M (tsize left + tsize right + 1) (max (tdepth left) (tdepth right) + 1)

instance Show a => Show (Tree a) where
  show Leaf = "()"
  show (Branch _ left x right) =
    show x
      ++ "\n"
      ++ formattedShow left "  |" False
      ++ "\n"
      ++ formattedShow right "  " True
      ++ "\n"
    where
      formattedShow tree pref isR =
        case tree of
          Leaf ->
            pref
              ++ (if isR
                    then "|"
                    else "")
              ++ "__()"
          (Branch _ l y r) ->
            pref
              ++ (if isR
                    then "|"
                    else "")
              ++ "__"
              ++ show y
              ++ "\n"
              ++ formattedShow l (pref ++ "   |") False
              ++ "\n"
              ++ formattedShow r (pref ++ "   ") True