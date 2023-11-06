{-# LANGUAGE FlexibleContexts #-}

module HW3.T3 
    ( joinOption
    , lawAssociativityOption
    , lawLeftIdentityOption
    , lawRightIdentityOption
    )
where

import HW3.T1
import HW3.T2

-- Option
joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some innerOption) = innerOption

-- Associativity Law for Option
lawAssociativityOption :: Eq (Option a) => Option (Option (Option a)) -> Bool
lawAssociativityOption m = joinOption (joinOption m) == joinOption (mapOption joinOption m)

-- Left Identity Law for Option
lawLeftIdentityOption :: Eq (Option a) => Option a -> Bool
lawLeftIdentityOption m = joinOption (wrapOption m) == m

-- Right Identity Law for Option
lawRightIdentityOption :: Eq (Option a) => Option a -> Bool
lawRightIdentityOption m = joinOption (mapOption wrapOption m) == m