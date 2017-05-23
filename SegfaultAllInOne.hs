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

import Generics.SOP hiding (I(..), SOP(..))

{-
import Data.Proxy
import qualified Data.Vector as V
import GHC.Exts (Any, Constraint)
import Unsafe.Coerce
-}

class MyShow a where
  myShow :: a -> String

instance MyShow Char where
  myShow _ = "X"

gshowS :: (All2 MyShow xss) => SOP I xss -> String
gshowS (SOP (Z xs))  = gshowP xs
gshowS (SOP (S xss)) = gshowS (SOP xss)

gshowP :: (All MyShow xs) => NP I xs -> String
gshowP Nil         = ""
gshowP (I x :* xs) = myShow x ++ (gshowP xs)

newtype I (a :: *) = I a

newtype SOP (f :: (k -> *)) (xss :: [[k]]) = SOP (NS (NP f) xss)

{-
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

class (SListI xs, AllF c xs) => All (c :: k -> Constraint) (xs :: [k]) where

  -- | General constrained eliminator for type-level lists.
  --
  -- Takes an argument for the case of the empty list, and an argument
  -- for the case of the non-empty list. The non-empty case can make use
  -- of the constraint for the head, and of an 'All' constraint on the
  -- tail.
  --
  cpara_All ::
       Proxy c
    -> r '[]
    -> (forall y ys . (c y, All c ys) => r ys -> r (y ': ys))
    -> r xs

instance All c '[] where
  cpara_All _ nil _cons = nil
  {-# INLINE cpara_All #-}

instance (c x, All c xs) => All c (x ': xs) where
  cpara_All p nil cons = cons (cpara_All p nil cons)
  {-# INLINE cpara_All #-}

type family AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance AllF _c '[]       = ()
type instance AllF  c (x ': xs) = (c x, All c xs)

type All2 f = All (All f)

type SListI = All Top

type SListI2 = All SListI

class Top x
instance Top x
-}


main :: IO ()
main = do
  let t = I 'x' :* Nil
  print (gshowS (SOP (Z t) :: SOP I '[ '[ Char ] ]))
