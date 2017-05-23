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

data IsNP (xs :: [*]) where
  IsNil  :: IsNP '[]
  IsCons :: x -> NP xs -> IsNP (x ': xs)

isNP :: NP xs -> IsNP xs
isNP (NP xs) =
  if null xs
    then unsafeCoerce IsNil
    else unsafeCoerce (IsCons (head xs) (NP (tail xs)))

pattern Nil :: () => (xs ~ '[]) => NP xs
pattern Nil <- (isNP -> IsNil)
  where
    Nil = NP []

pattern (:*) :: () => (xs' ~ (x ': xs)) => x -> NP xs -> NP xs'
pattern x :* xs <- (isNP -> IsCons x xs)
  where
    x :* NP xs = NP (unsafeCoerce x : xs)
infixr 5 :*

data NS (xss :: [[*]]) = forall xs . NS !Int (NP xs)

data IsNS (xs :: [[*]]) where
  IsZ :: NP x -> IsNS (x ': xs)
  IsS :: NS xs -> IsNS (x ': xs)

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
