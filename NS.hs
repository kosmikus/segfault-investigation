{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module NS where

import GHC.Exts (Any)
import Unsafe.Coerce

newtype NP (xs :: [*]) = NP [Any]

mkNP :: x -> NP '[ x ]
mkNP x = NP [unsafeCoerce x]

data IsNP (xs :: [*]) where
  IsNil  :: IsNP '[]
  IsCons :: x -> NP xs -> IsNP (x ': xs)

isNP :: NP xs -> IsNP xs
isNP (NP xs) =
  case xs of
    []       -> unsafeCoerce IsNil
    (x : xs) -> unsafeCoerce (IsCons x (NP xs))

data NS (xss :: [[*]]) = NS !Int Any

mkNS :: NP x -> NS '[ x ]
mkNS x = NS 0 (unsafeCoerce x)

data IsNS (xs :: [[*]]) where
  IsZ :: NP xs -> IsNS (xs ': xss)
  IsS :: NS xss -> IsNS (xs ': xss)

isNS :: NS xs -> IsNS xs
isNS (NS i x)
  | i == 0    = unsafeCoerce (IsZ (unsafeCoerce x))
  | otherwise = unsafeCoerce (IsS (NS (i - 1) x))

pattern Z :: () => (xs' ~ (x ': xs)) => NP x -> NS xs'
pattern Z x <- (isNS -> IsZ x)
  where
    Z x = NS 0 (unsafeCoerce x)

pattern S :: () => (xs' ~ (x ': xs)) => NS xs -> NS xs'
pattern S p <- (isNS -> IsS p)
