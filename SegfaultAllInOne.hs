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
module Main where

import NS

import GHC.Exts (Any, Constraint)
import qualified Data.Vector as V
import Unsafe.Coerce

class MyShow a where
  myShow :: a -> String

instance MyShow Char where
  myShow _ = "X"

gshowS :: (All2 MyShow xss) => NS (NP I) xss -> String
gshowS (Z xs)  = gshowP xs
gshowS (S xss) = gshowS xss

gshowP :: (All MyShow xs) => NP I xs -> String
gshowP (I x :* Nil) = myShow x
{-
gshowP Nil         = ""
gshowP (I x :* xs) = myShow x ++ (gshowP xs)
-}

newtype I (a :: *) = I a

newtype NP (f :: k -> *) (xs :: [k]) = NP (V.Vector (f Any))

data IsNP (f :: k -> *) (xs :: [k]) where
  IsNil  :: IsNP f '[]
  IsCons :: f x -> NP f xs -> IsNP f (x ': xs)

isNP :: NP f xs -> IsNP f xs
isNP (NP xs) =
  if V.null xs
    then unsafeCoerce IsNil
    else unsafeCoerce (IsCons (V.unsafeHead xs) (NP (V.unsafeTail xs)))

pattern Nil :: () => (xs ~ '[]) => NP f xs
pattern Nil <- (isNP -> IsNil)
  where
    Nil = NP V.empty

pattern (:*) :: () => (xs' ~ (x ': xs)) => f x -> NP f xs -> NP f xs'
pattern x :* xs <- (isNP -> IsCons x xs)
  where
    x :* NP xs = NP (V.cons (unsafeCoerce x) xs)
infixr 5 :*

{-
data NS (f :: k -> *) (xs :: [k]) = NS !Int (f Any)

data IsNS (f :: k -> *) (xs :: [k]) where
  IsZ :: f x -> IsNS f (x ': xs)
  IsS :: NS f xs -> IsNS f (x ': xs)

isNS :: NS f xs -> IsNS f xs
isNS (NS i x)
  | i == 0    = unsafeCoerce (IsZ x)
  | otherwise = unsafeCoerce (IsS (NS (i - 1) x))

pattern Z :: () => (xs' ~ (x ': xs)) => f x -> NS f xs'
pattern Z x <- (isNS -> IsZ x)
  where
    Z x = NS 0 (unsafeCoerce x)

pattern S :: () => (xs' ~ (x ': xs)) => NS f xs -> NS f xs'
pattern S p <- (isNS -> IsS p)
  where
    S (NS i x) = NS (i + 1) x
-}

class (AllF c xs) => All (c :: k -> Constraint) (xs :: [k])

instance All c '[]

instance (c x, All c xs) => All c (x ': xs)

type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

type All2 f = All (All f)

main :: IO ()
main = do
  let t = I 'x' :* Nil
  -- print (gshowX (S (Z (I 'x')) :: NS I '[ Char, Char ]))
  print (gshowS (Z t :: NS (NP I) '[ '[ Char ] ]))
