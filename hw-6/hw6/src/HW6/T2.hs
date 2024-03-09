{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet
  , Contains
  , Add
  , Delete
  ) where

import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains _ '[] = 'False
  Contains x (s : sx) = x == s || Contains x sx

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[] = '[]
  Delete x (s : sx) = If (x == s) sx (s : Delete x sx)

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add x '[] = '[ x]
  Add x (s : sx) = If (x == s) (s : sx) (s : Add x sx)